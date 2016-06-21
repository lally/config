{ mkDerivation, base, configurator, containers, hslogger, libX11
, mtl, process, regex-pcre, select, stdenv, stm, text, tuple, unix
, unordered-containers, X11, xmonad, xmonad-contrib, yesod
}:
mkDerivation {
  pname = "window-manager";
  version = "0.1.0.0";
  src = /home/lally/config/xmonad;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base configurator containers hslogger mtl process regex-pcre select
    stm text tuple unix unordered-containers X11 xmonad xmonad-contrib
    yesod
  ];
  librarySystemDepends = [ libX11 ];
  executableHaskellDepends = [
    base configurator containers hslogger mtl process regex-pcre select
    stm text tuple unix unordered-containers X11 xmonad xmonad-contrib
    yesod
  ];
  description = "Lally's xmonad setup";
  license = stdenv.lib.licenses.unfree;
  buildInputs = [ emacs zsh tmux man w3m ];
}
