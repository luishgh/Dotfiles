# Set up gaming hub

{ pkgs, ... }:
{
  # Games are mostly unfree :/
  nixpkgs.config.allowUnfree = true;

  # NVIDIA drivers
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.opengl.enable = true;

  # Extra OpenGL options
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  hardware.opengl.setLdLibraryPath = true;
  hardware.opengl.driSupport32Bit = true;

  # Steam
  hardware.steam-hardware.enable = true;
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
  };

  environment.systemPackages = with pkgs; [
    lutris
    discord
    heroic
    protonup
    vulkan-tools
  ];
}
