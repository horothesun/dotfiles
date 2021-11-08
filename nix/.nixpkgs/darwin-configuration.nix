{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs;
    [
      starship
      fzf
      ripgrep
      jq
      yq
      htop
      tldr
      lynx
      awscli
      k9s
#      wget
#      git
#      (sbt.override { jre = pkgs.jdk11; })
#      vim
#      stow
#      terraform
#      tmux
#      tree
#      unzip
#      curl
#      neovim
#      bash_5
#      nodejs_latest
#      stack
#      go
#      glow
#      terraform-lsp
#      texlab
#      jdk11
#      gradle
#      apacheHttpd
#      rnix-lsp
#      postgresql
#      coreutils

    ];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.nixpkgs/darwin-configuration.nix";

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina
  # programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # environment.variables.JAVA_HOME = "${pkgs.jdk11}/";
}
