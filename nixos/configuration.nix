{ config, pkgs, ... }:

{
  imports =
    [ 
      ./gaming.nix
      ./gnome.nix
      ./my-vim.nix
    ];

  # Enable flakes
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };


  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.utf8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "pt_BR.utf8";
    LC_IDENTIFICATION = "pt_BR.utf8";
    LC_MEASUREMENT = "pt_BR.utf8";
    LC_MONETARY = "pt_BR.utf8";
    LC_NAME = "pt_BR.utf8";
    LC_NUMERIC = "pt_BR.utf8";
    LC_PAPER = "pt_BR.utf8";
    LC_TELEPHONE = "pt_BR.utf8";
    LC_TIME = "pt_BR.utf8";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  services.xserver.libinput = {
    enable = true;
    touchpad.tapping = true;
    touchpad.naturalScrolling = true;
  };

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  services.flatpak.enable = true;

  users.users.luishgh = {
    isNormalUser = true;
    description = "Luis Henrique Gomes Higino";
    extraGroups = [ "networkmanager" "wheel" ];
  };

  environment.systemPackages = with pkgs; [
    git
    stow
    gnumake
    pass
    my-vim
    emacs
    gcc # for emacs
    firefox
    tdesktop # telegram
    spotify
  ];

  fonts.fonts = with pkgs; [
    # Noto
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    # Coding
    jetbrains-mono
    (iosevka-bin.override { variant = "aile"; })
    emacs-all-the-icons-fonts
  ];

  # Secret management tools
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  programs.browserpass.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leavecatenate(variables, "bootdev", bootdev)
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?

}
