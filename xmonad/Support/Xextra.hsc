{-# LANGUAGE ForeignFunctionInterface #-}

module Support.Xextra (listProperties, textPropertyToStringList) where
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

foreign import ccall unsafe "XlibExtras.h XListProperties"
    xListProperties :: Display -> Window -> Ptr CInt -> IO (Ptr Atom)

foreign import ccall unsafe "XlibExtras.h XTextPropertyToStringList"
    xTextPropertyToStringList :: Ptr TextProperty -> Ptr (Ptr CString) -> Ptr CInt -> IO (Status)

foreign import ccall unsafe "XlibExtras.h XFreeStringList"
    xFreeStringList :: Ptr CString -> IO ()

-- | Interface to XListProperties
listProperties :: Display -> Window -> IO [Atom]
listProperties dpy win = do
  let
    callX :: Ptr CInt -> IO [Atom]
    callX numP = do rawAtoms <- xListProperties dpy win numP
                    numC <- peek numP
                    let num = fromIntegral numC
                    arr <- peekArray num rawAtoms
                    xFree rawAtoms
                    return arr
  atoms <- alloca callX
  -- atomNames <- getAtomNames dpy atoms
  return atoms

textPropertyToStringList :: TextProperty -> IO [String]
textPropertyToStringList textProp = do
  alloca $ \textPropP -> do  -- pointer to textProp
  alloca $ \stringCountP -> do -- dest pointer for string count
  alloca $ \rawStringsP -> do -- destination pointer for strings
    poke textPropP textProp
    poke stringCountP 0
    status <- xTextPropertyToStringList textPropP rawStringsP stringCountP
    if status == 0
       then do
            -- not a text property atom.
            return []
       else do
            stringCount <- peek stringCountP
            rawStrings <- peek rawStringsP
            strings <- peekArray (fromIntegral $ stringCount) rawStrings
            results <- mapM peekCString strings
            let filteredResults = filter (\s -> (length s) > 0) results
            xFreeStringList rawStrings
            return filteredResults
