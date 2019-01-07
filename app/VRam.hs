module VRam where
    import Data.Word(Word8, Word16)
    import           Data.Vector.Unboxed ((!), (//))
    import qualified Data.Vector.Unboxed as V

    type Memory = V.Vector Word8
    type Address = Word16
    type Data = Word8

    memory :: Memory
    memory = V.replicate 0xFFFF 0

    read :: Memory -> Address -> Data
    read m a = m ! fromIntegral a

    write :: Memory -> Address -> Data -> Memory
    write m a v = m // [(fromIntegral a,v)]