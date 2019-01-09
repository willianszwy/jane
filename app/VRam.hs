module VRam where
    import Data.Word(Word8, Word16)
    import           Data.Vector.Unboxed ((!), (//))
    import qualified Data.Vector.Unboxed as V

    type Memory = V.Vector Word8
    type Address = Word16
    type Data = Word8

    data MemType a = RAM a | PPU a | APU a | OPB a | SRM a | PRG a deriving (Eq,Show)

    instance Functor MemType where  
        fmap f (RAM m) = RAM (f m)
        fmap f (PPU m) = PPU (f m)
        fmap f (APU m) = APU (f m)
        fmap f (OPB m) = OPB (f m)
        fmap f (SRM m) = SRM (f m)
        fmap f (PRG m) = PRG (f m)


    data VRam = VRam { 
        ram :: MemType Memory,
        ppu :: MemType Memory,
        apu :: MemType Memory,
        openBus :: MemType Memory,
        sram :: MemType Memory,
        programRom :: MemType Memory
      } deriving (Eq, Show)    
    

    init :: VRam 
    init = VRam {
        ram = RAM (V.replicate 0x800 0),
        ppu = PPU (V.replicate 0x8 0),
        apu = APU (V.replicate 0x18 0),
        openBus = OPB (V.replicate 0x1FE7 0),
        sram = SRM (V.replicate 0x2000 0),
        programRom = PRG (V.replicate 0x8000 0)
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

    addressMapper :: VRam -> Address -> MemType Memory
    addressMapper vram a
           | elem a [0..0x1FFF] = ram vram
           | elem a [0x2000..0x3FFF] = ppu vram
           | elem a [0x4000..0x4017] = apu vram
           | elem a [0x4018..0x5FFF] = openBus vram
           | elem a [0x6000..0x7FFF] = sram vram
           | elem a [0x8000..0xFFFF] = programRom vram


    fromMemType :: MemType Memory -> Memory
    fromMemType (RAM m) = m
    fromMemType (PPU m) = m
    fromMemType (APU m) = m
    fromMemType (OPB m) = m
    fromMemType (SRM m) = m
    fromMemType (PRG m) = m

    readMemory :: Memory -> Address -> Data
    readMemory m a = m ! fromIntegral a
       
    writeMemory :: Address -> Data -> Memory -> Memory
    writeMemory a d m = m // [(fromIntegral a,d)]

    update :: VRam -> MemType Memory -> VRam
    update vram (RAM m) = vram {ram = RAM m}
    update vram (PPU m) = vram {ppu = PPU m} 
    update vram (APU m) = vram {apu = APU m} 
    update vram (OPB m) = vram {openBus = OPB m} 
    update vram (SRM m) = vram {sram = SRM m} 
    update vram (PRG m) = vram {programRom = PRG m} 

    read :: VRam -> Address -> Data
    read vr a = readMemory (fromMemType $ addressMapper vr a) (addressMirror a)

    write :: VRam -> Address -> Data -> VRam
    write vr a d = update vr $ writeMemory (addressMirror a) d <$> addressMapper vr a