# doom.d

Own custom configuration files for Emacs' Doom flavour

## Emacs' Doom Installation

* Doom creator [link(https://github.com/hlissner/doom-emacs)

* Process taken from [Zaiste](https://www.youtube.com/channel/UCzgkOWKcwy0uhYilE6bd1Lg) Youtube channel.
Doom's [playlist](https://www.youtube.com/watch?v=rCMh7srOqvw)

1. $ git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
2. Check out the “develop” branch
3. There may be some problems installing undo-tree package due to TLS configuration to the package repos. 
* From https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/ 
* Copy `(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")` as the first line in `~/.emacs.d/bin/doom` file.
5. $ ~/.emacs.d/bin/doom quickstart
