module VRam where
    import Data.Word(Word8, Word16)
    import           Data.Vector.Unboxed ((!), (//))
    import qualified Data.Vector.Unboxed as V
    import qualified Data.ByteString as B

    type Memory = V.Vector Word8
    type Address = Word16
    type Data = Word8

    data MemPartition a = RAM a | PPU a | APU a | OPB a | SRM a | PRG a deriving (Eq,Show)

    instance Functor MemPartition where  
        fmap f (RAM m) = RAM (f m)
        fmap f (PPU m) = PPU (f m)
        fmap f (APU m) = APU (f m)
        fmap f (OPB m) = OPB (f m)
        fmap f (SRM m) = SRM (f m)
        fmap f (PRG m) = PRG (f m)


    data VRam = VRam { 
        ram :: MemPartition Memory,
        ppu :: MemPartition Memory,
        apu :: MemPartition Memory,
        openBus :: MemPartition Memory,
        sram :: MemPartition Memory,
        programRom :: MemPartition Memory
      } deriving (Eq, Show)    
    

    init :: VRam 
    init = VRam {
        ram = RAM (V.replicate 0x800 0),
        ppu = PPU (V.replicate 0x8 0),
        apu = APU (V.replicate 0x18 0),
        openBus = OPB (V.replicate 0x1FE7 0),
        sram = SRM (V.replicate 0x2000 0),
        programRom = PRG (V.empty)
    }

    addressMirror :: Address -> Address
    addressMirror address 
           | elem address [0..0x1FFF] = mod address 0x800
           | elem address [0x2000..0x3FFF] = mod address 0x8
           | elem address [0x4000..0x4017] = mod address 0x4000
           | elem address [0x4018..0x5FFF] = mod address 0x4018
           | elem address [0x6000..0x7FFF] = mod address 0x2000
           | elem address [0x8000..0xFFFF] = mod address 0x8000
    addressMirror address = address

    addressPartitionMapper :: VRam -> Address -> MemPartition Memory
    addressPartitionMapper vram a
           | elem a [0..0x1FFF] = ram vram
           | elem a [0x2000..0x3FFF] = ppu vram
           | elem a [0x4000..0x4017] = apu vram
           | elem a [0x4018..0x5FFF] = openBus vram
           | elem a [0x6000..0x7FFF] = sram vram
           | elem a [0x8000..0xFFFF] = programRom vram


    fromMemPartition :: MemPartition Memory -> Memory
    fromMemPartition (RAM m) = m
    fromMemPartition (PPU m) = m
    fromMemPartition (APU m) = m
    fromMemPartition (OPB m) = m
    fromMemPartition (SRM m) = m
    fromMemPartition (PRG m) = m

    readMemory :: Memory -> Address -> Data
    readMemory m a = m ! fromIntegral a
       
    writeMemory :: Address -> Data -> Memory -> Memory
    writeMemory a d m = m // [(fromIntegral a,d)]

    update :: VRam -> MemPartition Memory -> VRam
    update vram (RAM m) = vram {ram = RAM m}
    update vram (PPU m) = vram {ppu = PPU m} 
    update vram (APU m) = vram {apu = APU m} 
    update vram (OPB m) = vram {openBus = OPB m} 
    update vram (SRM m) = vram {sram = SRM m} 
    update vram (PRG m) = vram {programRom = PRG m} 

    read :: VRam -> Address -> Data
    read vr a = readMemory (fromMemPartition $ addressPartitionMapper vr a) (addressMirror a)

    write :: VRam -> Address -> Data -> VRam
    write vr a d = update vr $ writeMemory (addressMirror a) d <$> addressPartitionMapper vr a

    loadProgramRom :: VRam -> B.ByteString -> VRam
    loadProgramRom vram bs = 
        update vram (PRG (V.generate (B.length bs) (\i -> B.index bs i )))

    slice :: Address -> Int -> VRam -> [Data]
    slice i n vr = V.toList $ V.slice (fromIntegral $ addressMirror i) n (fromMemPartition $ addressPartitionMapper vr i)