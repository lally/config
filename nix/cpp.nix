with import <nixpkgs> {};

# Taken from a combination of
# https://bluishcoder.co.nz/2014/05/15/firefox-development-on-nixos.html
# and
# http://sandervanderburg.blogspot.com/2013/12/using-nix-while-doing-development.html

myEnvFun {
  name = "cpp";
  buildInputs = [
    stdenv pkgconfig gtk glib gobjectIntrospection
    dbus_libs dbus glib alsaLib gcc xlibs.libXrender
    xlibs.libX11 xlibs.libXext xlibs.libXft xlibs.libXt
    ats pango freetype fontconfig gdk_pixbuf cairo python
    git autoconf213 unzip zip yasm alsaLib dbus_libs which atk
    gstreamer gst_plugins_base pulseaudio
  ];

  extraCmds = ''
   export C_INCLUDE_PATH=${dbus_libs}/include/dbus-1.0:${dbus_libs}/lib/dbus-1.0/include
   export CPLUS_INCLUDE_PATH=${dbus_libs}/include/dbus-1.0:${dbus_libs}/lib/dbus-1.0/include
   LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:${gcc.gcc}/lib64
   for i in $nativeBuildInputs; do
     LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:\$i/lib
   done
   export LD_LIBRARY_PATH
   export AUTOCONF=autoconf
  '';
}
