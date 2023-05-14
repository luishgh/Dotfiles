{
  description = "My gaming hub configuration.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
  };

  outputs = { self, nixpkgs }: {
    nixosConfigurations.LenovoNixOS = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ 
        ./hosts/lenovo-legion.nix
        ./configuration.nix 
      ];
    };
  };
}
