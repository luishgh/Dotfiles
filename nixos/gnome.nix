# Set up GNOME

{ pkgs, ... }:
{
  # Enable the Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  environment.systemPackages = with pkgs; [
    gnome.gnome-tweaks
  ];
}


