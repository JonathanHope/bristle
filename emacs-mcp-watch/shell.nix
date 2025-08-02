{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    nodejs_20
    nodePackages.prettier
    nodePackages.typescript
    nodePackages.typescript-language-server
  ];
}
