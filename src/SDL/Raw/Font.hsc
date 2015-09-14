{-|

Module      : SDL.Raw.Font
Copyright   : (c) 2015 Siniša Biđin
License     : MIT
Maintainer  : sinisa@bidin.eu
Stability   : experimental

Raw bindings to the @SDL2_ttf@ library. No error-handling is done here. For more
information about specific function behaviour, see the @SDL2_ttf@ documentation.

-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module SDL.Raw.Font
  (
  -- * General
    init
  , wasInit
  , quit
  , getVersion

  -- * Loading fonts
  , Font
  , FontPath
  , PointSize
  , openFont
  , Free
  , openFont_RW
  , Index
  , openFontIndex
  , openFontIndex_RW
  , closeFont

  -- * Font attributes
  , getFontStyle
  , setFontStyle
  , pattern TTF_STYLE_NORMAL
  , pattern TTF_STYLE_BOLD
  , pattern TTF_STYLE_ITALIC
  , pattern TTF_STYLE_UNDERLINE
  , pattern TTF_STYLE_STRIKETHROUGH
  , getFontOutline
  , setFontOutline
  , getFontHinting
  , setFontHinting
  , pattern TTF_HINTING_NORMAL
  , pattern TTF_HINTING_LIGHT
  , pattern TTF_HINTING_MONO
  , pattern TTF_HINTING_NONE
  , getFontKerning
  , setFontKerning
  , fontHeight
  , fontAscent
  , fontDescent
  , fontLineSkip
  , fontFaces
  , fontFaceIsFixedWidth
  , fontFaceFamilyName
  , fontFaceStyleName
  , glyphIsProvided
  , glyphMetrics

  -- * Getting text size
  , sizeText
  , sizeUTF8
  , sizeUNICODE

  -- * Rendering text
  , renderText_Solid
  , renderText_Shaded
  , renderText_Blended
  , renderUTF8_Solid
  , renderUTF8_Shaded
  , renderUTF8_Blended
  , renderUNICODE_Solid
  , renderUNICODE_Shaded
  , renderUNICODE_Blended
  , renderGlyph_Solid
  , renderGlyph_Shaded
  , renderGlyph_Blended

  -- * Other
  , byteSwappedUNICODE
  , pattern UNICODE_BOM_NATIVE
  , pattern UNICODE_BOM_SWAPPED
  ) where

#include "SDL_ttf.h"

import Control.Monad.IO.Class
import Foreign.C.String       (CString)
import Foreign.C.Types        (CInt(..), CLong(..), CUShort(..))
import Foreign.Ptr            (Ptr)
import Prelude         hiding (init)
import SDL.Raw.Types          (Version, Surface, RWops, Color)

pattern UNICODE_BOM_NATIVE      = #{const UNICODE_BOM_NATIVE}
pattern UNICODE_BOM_SWAPPED     = #{const UNICODE_BOM_SWAPPED}
pattern TTF_STYLE_NORMAL        = #{const TTF_STYLE_NORMAL}
pattern TTF_STYLE_BOLD          = #{const TTF_STYLE_BOLD}
pattern TTF_STYLE_ITALIC        = #{const TTF_STYLE_ITALIC}
pattern TTF_STYLE_UNDERLINE     = #{const TTF_STYLE_UNDERLINE}
pattern TTF_STYLE_STRIKETHROUGH = #{const TTF_STYLE_STRIKETHROUGH}
pattern TTF_HINTING_LIGHT       = #{const TTF_HINTING_LIGHT}
pattern TTF_HINTING_MONO        = #{const TTF_HINTING_MONO}
pattern TTF_HINTING_NONE        = #{const TTF_HINTING_NONE}
pattern TTF_HINTING_NORMAL      = #{const TTF_HINTING_NORMAL}

foreign import ccall safe "static TTF_Linked_Version" getVersion' :: IO (Ptr Version)
{-# INLINE getVersion #-}
getVersion :: forall m_a5cM. MonadIO m_a5cM => m_a5cM (Ptr Version)
getVersion = liftIO getVersion'

foreign import ccall safe "static TTF_Init" init' :: IO CInt
{-# INLINE init #-}
init :: forall m_a5dQ. MonadIO m_a5dQ => m_a5dQ CInt
init = liftIO init'

foreign import ccall safe "static TTF_WasInit" wasInit' :: IO CInt
{-# INLINE wasInit #-}
wasInit :: forall m_a5pE. MonadIO m_a5pE => m_a5pE CInt
wasInit = liftIO wasInit'

foreign import ccall safe "static TTF_Quit" quit' :: IO ()
{-# INLINE quit #-}
quit :: forall m_a5qh. MonadIO m_a5qh => m_a5qh ()
quit = liftIO quit'

-- | A path to a font file.
type FontPath = CString

-- | Point size (based on 72DPI). Translates to pixel height.
type PointSize = CInt

-- | The raw, underlying @TTF_Font@ struct.
data Font

-- | Should the 'Ptr' 'RWops' be freed after an operation? 1 for yes, 0 for no.
type Free = CInt

-- | Indicates the font face we're loading. First face is always 0.
type Index = CLong

foreign import ccall safe "static TTF_OpenFont" openFont' :: FontPath -> PointSize -> IO (Ptr Font)
{-# INLINE openFont #-}
openFont x_a5rh x_a5ri = liftIO (openFont' x_a5rh x_a5ri)

foreign import ccall safe "static TTF_OpenFontRW" openFont_RW' :: Ptr RWops -> Free -> PointSize -> IO (Ptr Font)
{-# INLINE openFont_RW #-}
openFont_RW x_a5sC x_a5sD x_a5sE = liftIO (openFont_RW' x_a5sC x_a5sD x_a5sE)

foreign import ccall safe "static TTF_OpenFontIndex" openFontIndex' :: FontPath -> PointSize -> Index -> IO (Ptr Font)
{-# INLINE openFontIndex #-}
openFontIndex x_a5tQ x_a5tR x_a5tS = liftIO (openFontIndex' x_a5tQ x_a5tR x_a5tS)

foreign import ccall safe "static TTF_OpenFontIndexRW" openFontIndex_RW' :: Ptr RWops -> Free -> PointSize -> Index -> IO (Ptr Font)
{-# INLINE openFontIndex_RW #-}
openFontIndex_RW x_a5vg x_a5vh x_a5vi x_a5vj = liftIO (openFontIndex_RW' x_a5vg x_a5vh x_a5vi x_a5vj)

foreign import ccall safe "static TTF_CloseFont" closeFont' :: Ptr Font -> IO ()
{-# INLINE closeFont #-}
closeFont x_a5wf = liftIO (closeFont' x_a5wf)

foreign import ccall safe "static TTF_ByteSwappedUNICODE" byteSwappedUNICODE' :: CInt -> IO ()
{-# INLINE byteSwappedUNICODE #-}
byteSwappedUNICODE x_a5x1 = liftIO (byteSwappedUNICODE' x_a5x1)

foreign import ccall safe "static TTF_GetFontStyle" getFontStyle' :: Ptr Font -> IO CInt
{-# INLINE getFontStyle #-}
getFontStyle x_a5xV = liftIO (getFontStyle' x_a5xV)

foreign import ccall safe "static TTF_SetFontStyle" setFontStyle' :: Ptr Font -> CInt -> IO ()
{-# INLINE setFontStyle #-}
setFontStyle x_a5yU x_a5yV = liftIO (setFontStyle' x_a5yU x_a5yV)

foreign import ccall safe "static TTF_GetFontOutline" getFontOutline' :: Ptr Font -> IO CInt
{-# INLINE getFontOutline #-}
getFontOutline x_a5zR = liftIO (getFontOutline' x_a5zR)

foreign import ccall safe "static TTF_SetFontOutline" setFontOutline' :: Ptr Font -> CInt -> IO ()
{-# INLINE setFontOutline #-}
setFontOutline x_a5AQ x_a5AR = liftIO (setFontOutline' x_a5AQ x_a5AR)

foreign import ccall safe "static TTF_GetFontHinting" getFontHinting' :: Ptr Font -> IO CInt
{-# INLINE getFontHinting #-}
getFontHinting x_a5BN = liftIO (getFontHinting' x_a5BN)

foreign import ccall safe "static TTF_SetFontHinting" setFontHinting' :: Ptr Font -> CInt -> IO ()
{-# INLINE setFontHinting #-}
setFontHinting x_a5CM x_a5CN = liftIO (setFontHinting' x_a5CM x_a5CN)

foreign import ccall safe "static TTF_GetFontKerning" getFontKerning' :: Ptr Font -> IO CInt
{-# INLINE getFontKerning #-}
getFontKerning x_a5DJ = liftIO (getFontKerning' x_a5DJ)

foreign import ccall safe "static TTF_SetFontKerning" setFontKerning' :: Ptr Font -> CInt -> IO ()
{-# INLINE setFontKerning #-}
setFontKerning x_a5EI x_a5EJ = liftIO (setFontKerning' x_a5EI x_a5EJ)

foreign import ccall safe "static TTF_FontHeight" fontHeight' :: Ptr Font -> IO CInt
{-# INLINE fontHeight #-}
fontHeight x_a5FF = liftIO (fontHeight' x_a5FF)

foreign import ccall safe "static TTF_FontAscent" fontAscent' :: Ptr Font -> IO CInt
{-# INLINE fontAscent #-}
fontAscent x_a5GA = liftIO (fontAscent' x_a5GA)

foreign import ccall safe "static TTF_FontDescent" fontDescent' :: Ptr Font -> IO CInt
{-# INLINE fontDescent #-}
fontDescent x_a5Hv = liftIO (fontDescent' x_a5Hv)

foreign import ccall safe "static TTF_FontLineSkip" fontLineSkip' :: Ptr Font -> IO CInt
{-# INLINE fontLineSkip #-}
fontLineSkip x_a5Iq = liftIO (fontLineSkip' x_a5Iq)

foreign import ccall safe "static TTF_FontFaces" fontFaces' :: Ptr Font -> IO CLong
{-# INLINE fontFaces #-}
fontFaces x_a5Jl = liftIO (fontFaces' x_a5Jl)

foreign import ccall safe "static TTF_FontFaceIsFixedWidth" fontFaceIsFixedWidth' :: Ptr Font -> IO CInt
{-# INLINE fontFaceIsFixedWidth #-}
fontFaceIsFixedWidth x_a5Kg = liftIO (fontFaceIsFixedWidth' x_a5Kg)

foreign import ccall safe "static TTF_FontFaceFamilyName" fontFaceFamilyName' :: Ptr Font -> IO CString
{-# INLINE fontFaceFamilyName #-}
fontFaceFamilyName x_a5Lb = liftIO (fontFaceFamilyName' x_a5Lb)

foreign import ccall safe "static TTF_FontFaceStyleName" fontFaceStyleName' :: Ptr Font -> IO CString
{-# INLINE fontFaceStyleName #-}
fontFaceStyleName x_a5M6 = liftIO (fontFaceStyleName' x_a5M6)

foreign import ccall safe "static TTF_GlyphIsProvided" glyphIsProvided' :: Ptr Font -> CUShort -> IO CInt
{-# INLINE glyphIsProvided #-}
glyphIsProvided x_a5N8 x_a5N9 = liftIO (glyphIsProvided' x_a5N8 x_a5N9)

foreign import ccall safe "static TTF_GlyphMetrics" glyphMetrics' :: Ptr Font -> CUShort -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
{-# INLINE glyphMetrics #-}
glyphMetrics x_a5Zw x_a5Zx x_a5Zy x_a5Zz x_a5ZA x_a5ZB x_a5ZC = liftIO (glyphMetrics' x_a5Zw x_a5Zx x_a5Zy x_a5Zz x_a5ZA x_a5ZB x_a5ZC)

foreign import ccall safe "static TTF_SizeText" sizeText' :: Ptr Font -> CString -> Ptr CInt -> Ptr CInt -> IO CInt
{-# INLINE sizeText #-}
sizeText x_a61f x_a61g x_a61h x_a61i = liftIO (sizeText' x_a61f x_a61g x_a61h x_a61i)

foreign import ccall safe "static TTF_SizeUTF8" sizeUTF8' :: Ptr Font -> CString -> Ptr CInt -> Ptr CInt -> IO CInt
{-# INLINE sizeUTF8 #-}
sizeUTF8 x_a62P x_a62Q x_a62R x_a62S = liftIO (sizeUTF8' x_a62P x_a62Q x_a62R x_a62S)

foreign import ccall safe "static TTF_SizeUNICODE" sizeUNICODE' :: Ptr Font -> Ptr CUShort -> Ptr CInt -> Ptr CInt -> IO CInt
{-# INLINE sizeUNICODE #-}
sizeUNICODE x_a64v x_a64w x_a64x x_a64y = liftIO (sizeUNICODE' x_a64v x_a64w x_a64x x_a64y)

foreign import ccall safe "static TTF_RenderText_Solid_p" renderText_Solid' :: Ptr Font -> CString -> Ptr Color -> IO (Ptr Surface)
{-# INLINE renderText_Solid #-}
renderText_Solid x_a65Z x_a660 x_a661 = liftIO (renderText_Solid' x_a65Z x_a660 x_a661)

foreign import ccall safe "static TTF_RenderUTF8_Solid_p" renderUTF8_Solid' :: Ptr Font -> CString -> Ptr Color -> IO (Ptr Surface)
{-# INLINE renderUTF8_Solid #-}
renderUTF8_Solid x_a67q x_a67r x_a67s = liftIO (renderUTF8_Solid' x_a67q x_a67r x_a67s)

foreign import ccall safe "static TTF_RenderUNICODE_Solid_p" renderUNICODE_Solid' :: Ptr Font -> Ptr CUShort -> Ptr Color -> IO (Ptr Surface)
{-# INLINE renderUNICODE_Solid #-}
renderUNICODE_Solid x_a68X x_a68Y x_a68Z = liftIO (renderUNICODE_Solid' x_a68X x_a68Y x_a68Z)

foreign import ccall safe "static TTF_RenderGlyph_Solid_p" renderGlyph_Solid' :: Ptr Font -> CUShort -> Ptr Color -> IO (Ptr Surface)
{-# INLINE renderGlyph_Solid #-}
renderGlyph_Solid x_a6ap x_a6aq x_a6ar = liftIO (renderGlyph_Solid' x_a6ap x_a6aq x_a6ar)

foreign import ccall safe "static TTF_RenderText_Shaded_p" renderText_Shaded' :: Ptr Font -> CString -> Ptr Color -> Ptr Color -> IO (Ptr Surface)
{-# INLINE renderText_Shaded #-}
renderText_Shaded x_a6c3 x_a6c4 x_a6c5 x_a6c6 = liftIO (renderText_Shaded' x_a6c3 x_a6c4 x_a6c5 x_a6c6)

foreign import ccall safe "static TTF_RenderUTF8_Shaded_p" renderUTF8_Shaded' :: Ptr Font -> CString -> Ptr Color -> Ptr Color -> IO (Ptr Surface)
{-# INLINE renderUTF8_Shaded #-}
renderUTF8_Shaded x_a6dK x_a6dL x_a6dM x_a6dN = liftIO (renderUTF8_Shaded' x_a6dK x_a6dL x_a6dM x_a6dN)

foreign import ccall safe "static TTF_RenderUNICODE_Shaded_p" renderUNICODE_Shaded' :: Ptr Font -> Ptr CUShort -> Ptr Color -> Ptr Color -> IO (Ptr Surface)
{-# INLINE renderUNICODE_Shaded #-}
renderUNICODE_Shaded x_a6fx x_a6fy x_a6fz x_a6fA = liftIO (renderUNICODE_Shaded' x_a6fx x_a6fy x_a6fz x_a6fA)

foreign import ccall safe "static TTF_RenderGlyph_Shaded_p" renderGlyph_Shaded' :: Ptr Font -> CUShort -> Ptr Color -> Ptr Color -> IO (Ptr Surface)
{-# INLINE renderGlyph_Shaded #-}
renderGlyph_Shaded x_a6hf x_a6hg x_a6hh x_a6hi = liftIO (renderGlyph_Shaded' x_a6hf x_a6hg x_a6hh x_a6hi)

foreign import ccall safe "static TTF_RenderText_Blended_p" renderText_Blended' :: Ptr Font -> CString -> Ptr Color -> IO (Ptr Surface)
{-# INLINE renderText_Blended #-}
renderText_Blended x_a6iJ x_a6iK x_a6iL = liftIO (renderText_Blended' x_a6iJ x_a6iK x_a6iL)

foreign import ccall safe "static TTF_RenderUTF8_Blended_p" renderUTF8_Blended' :: Ptr Font -> CString -> Ptr Color -> IO (Ptr Surface)
{-# INLINE renderUTF8_Blended #-}
renderUTF8_Blended x_a6ka x_a6kb x_a6kc = liftIO (renderUTF8_Blended' x_a6ka x_a6kb x_a6kc)

foreign import ccall safe "static TTF_RenderUNICODE_Blended_p" renderUNICODE_Blended' :: Ptr Font -> Ptr CUShort -> Ptr Color -> IO (Ptr Surface)
{-# INLINE renderUNICODE_Blended #-}
renderUNICODE_Blended x_a6lH x_a6lI x_a6lJ = liftIO (renderUNICODE_Blended' x_a6lH x_a6lI x_a6lJ)

foreign import ccall safe "static TTF_RenderGlyph_Blended_p" renderGlyph_Blended' :: Ptr Font -> CUShort -> Ptr Color -> IO (Ptr Surface)
{-# INLINE renderGlyph_Blended #-}
renderGlyph_Blended x_a6n9 x_a6na x_a6nb = liftIO (renderGlyph_Blended' x_a6n9 x_a6na x_a6nb)
