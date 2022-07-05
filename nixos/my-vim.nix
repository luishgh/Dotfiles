{ config, pkgs, ... }:
{
  nixpkgs.overlays = [ 
    (final: super: {

      my-vim = super.vim_configurable.customize {
          # Specifies the vim binary name.
          name = "vim";

          vimrcConfig.customRC = ''
            syntax enable
            set number relativenumber
          '';

          vimrcConfig.packages.myVimPackage = with super.vimPlugins; {
            # loaded on launch
            start = [ vim-nix vim-sensible ];
            # manually loadable by calling `:packadd $plugin-name`
            # opt = [ ];
            # To automatically load a plugin when opening a filetype, add vimrc lines like:
            # autocmd FileType php :packadd phpCompletion
          };
        };
      }
    ) 
  ];
}
