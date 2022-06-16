with import <nixpkgs> {}; # bring all of Nixpkgs into scope

vim_configurable.customize {
  # Specifies the vim binary name.
  name = "vim";

  vimrcConfig.customRC = ''
      syntax enable
  '';

  vimrcConfig.packages.myVimPackage = with pkgs.vimPlugins; {
    # loaded on launch
    start = [ vim-nix vim-sensible ];
    # manually loadable by calling `:packadd $plugin-name`
    # opt = [ ];
    # To automatically load a plugin when opening a filetype, add vimrc lines like:
    # autocmd FileType php :packadd phpCompletion
  };
}
