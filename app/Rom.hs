module Rom where

import qualified Data.ByteString               as B
import           Data.Binary.Strict.BitGet     as BG
import           Data.Int
import           Data.Binary.Strict.Get        as G
import           Data.Word

data INes = INes {
    header :: B.ByteString ,
    prg_rom :: Word8,
    chr_rom :: Word8,
    mapper_lower :: Word8,
    four_screen :: Bool,
    trainer :: Bool,
    battery_ram :: Bool,
    mirroring :: Bool,
    mapper_upper :: Word8,
    is_nes_2 :: Word8,
    playchoice :: Bool ,
    vs_unisystem :: Bool
    } deriving (Show)


loadRom :: FilePath -> IO B.ByteString
loadRom = B.readFile

parserINesHeader :: G.Get INes
parserINesHeader = do
    header <- G.getByteString 16
    let r = BG.runBitGet
            header
            (do
                header       <- getLeftByteString 32
                prg_rom      <- BG.getWord8
                chr_rom      <- BG.getWord8
                mapper_lower <- BG.getAsWord8 4
                four_screen  <- BG.getBit
                trainer      <- BG.getBit
                battery_ram  <- BG.getBit
                mirroring    <- BG.getBit
                mapper_upper <- BG.getAsWord8 4
                is_nes_2     <- BG.getAsWord8 2
                playchoice   <- BG.getBit
                INes header
                     prg_rom
                     chr_rom
                     mapper_lower
                     four_screen
                     trainer
                     battery_ram
                     mirroring
                     mapper_upper
                     is_nes_2
                     playchoice
                    <$> BG.getBit
            )
    case r of
        Left  error -> fail error
        Right x     -> return x

parseRom :: B.ByteString -> Either String INes
parseRom input = case fst $ (G.runGet parserINesHeader input) of
    Left  error -> fail error
    Right x     -> return x
