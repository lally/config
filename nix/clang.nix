   let
     pkgs = import <nixpkgs> {};
     stdenv = pkgs.stdenv;
   in rec {
     clangEnv = with pkgs; stdenv.mkDerivation rec {
       name = "clang-env";
       version = "1.1.1.1";
       src = ./.;
       buildInputs = [ clang gdb zsh tmux ncurses man
	    stdenv pkgconfig gtk glib gobjectIntrospection
	    dbus_libs dbus glib alsaLib xlibs.libXrender
	    xlibs.libX11 xlibs.libXext xlibs.libXft xlibs.libXt
	    ats pango freetype fontconfig gdk_pixbuf cairo python
	    git autoconf213 unzip zip yasm alsaLib dbus_libs which atk
	    gstreamer gst_plugins_base 
            boost
             #pulseaudio gcc
	];
     };
   }

