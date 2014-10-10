import IPC
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
 
main = do
 	-- initialize Gtk
 	_ <- initGUI
        runIPCServer
 
 	-- create a new window, a scrolled window, and a new webview
 	w  <- windowNew
 	sw <- scrolledWindowNew Nothing Nothing
 	wv <- webViewNew
 
 	-- set the child of the parent to the scrolled window,
 	-- and set some others attributes
 	set w [ containerChild       := sw
 	      , windowDefaultWidth   := 500
 	      , windowDefaultHeight  := 400
 	      , containerBorderWidth := 2
 	      ]
 	-- set the child of the scrolled windows to the webview.
 	set sw [ containerChild := wv ]
 
 	-- load slashdot.org on the webview.
 	webViewLoadUri wv "http://d3js.org"
 
 	-- on destroying event, we quit the mainloop
 	onDestroy w mainQuit
 	-- show all widgets starting from the root window
 	widgetShowAll w
 	-- start GTK main loop
 	mainGUI

