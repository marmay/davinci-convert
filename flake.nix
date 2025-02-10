{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: _prev: {
          # This overlay adds our project to pkgs
          davinci-convert-project =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc966";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = {};
                hlint = {};
                haskell-language-server = {};
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
              ];
              shell.nativeBuildInputs = with pkgs; [
                ffmpeg
              ];
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              # shell.crossPlatforms = p: [p.ghcjs];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.davinci-convert-project.flake {
        # This adds support for `nix build .#js-unknown-ghcjs:davinci-convert:exe:davinci-convert`
        # crossPlatforms = p: [p.javascript];
      };
    in flake // {
      # Built by `nix build .`
      packages.default = flake.packages."davinci-convert:exe:davinci-convert";

      # NixOS module
      nixosModules.default = { config, lib, pkgs, ... }: {
        options.services.davinci-convert = {
          enable = lib.mkEnableOption "Enable the Davinci Convert service";
          ffmpegPath = lib.mkOption {
            type = lib.types.str;
            default = "${pkgs.ffmpeg}/bin/ffmpeg";
            description = "Path to the ffmpeg executable";
          };
          port = lib.mkOption {
            type = lib.types.port;
            default = 3000;
            description = "Port to run the Davinci Convert service on";
          };
          basePath = lib.mkOption {
            type = lib.types.str;
            default = "/convert";
            description = "Base path for the Davinci Convert service";
          };
          fileBase = lib.mkOption {
            type = lib.types.str;
            default = "/var/lib/davinci-convert";
            description = "Base directory for uploads, tmp, and converted files";
          };
          nginx = {
            enable = lib.mkEnableOption "Enable Nginx reverse proxy";
            host = lib.mkOption {
              type = lib.types.str;
              default = "localhost";
              description = "Hostname for the Nginx reverse proxy";
            };
          };
        };

        config = lib.mkIf config.services.davinci-convert.enable {
          # Davinci Convert service
          systemd.services.davinci-convert = {
            description = "Davinci Convert Service";
            wantedBy = [ "multi-user.target" ];
            after = [ "network.target" ];
            serviceConfig = {
              ExecStart = "${flake.packages."davinci-convert:exe:davinci-convert"}/bin/davinci-convert --ffmpeg-path ${config.services.davinci-convert.ffmpegPath} --port ${toString config.services.davinci-convert.port} --base-path ${config.services.davinci-convert.basePath} --file-base ${config.services.davinci-convert.fileBase}";
              Restart = "always";
              User = "davinci-convert";
              Group = "davinci-convert";
              RuntimeDirectory = "davinci-convert";
              RuntimeDirectoryMode = "0755";
            };
          };

          # Create the file base directory
          systemd.tmpfiles.rules = [
            "d ${config.services.davinci-convert.fileBase} 0755 davinci-convert nginx - -"
            "d ${config.services.davinci-convert.fileBase}/uploads 0755 davinci-convert nginx - -"
            "d ${config.services.davinci-convert.fileBase}/tmp 0755 davinci-convert nginx - -"
            "d ${config.services.davinci-convert.fileBase}/converted 0755 davinci-convert nginx - -"
          ];

          # Nginx reverse proxy
          services.nginx = lib.mkIf config.services.davinci-convert.nginx.enable {
            enable = true;
            virtualHosts.${config.services.davinci-convert.nginx.host} = {
              locations."/uploads/" = {
                alias = "${config.services.davinci-convert.fileBase}/uploads/";
		extraConfig = ''
		  autoindex on;
		'';
              };
              locations."/tmp/" = {
                alias = "${config.services.davinci-convert.fileBase}/tmp/";
		extraConfig = ''
		  autoindex on;
		'';
              };
              locations."/converted/" = {
                alias = "${config.services.davinci-convert.fileBase}/converted/";
		extraConfig = ''
		  autoindex on;
		'';
              };
              locations."/" = {
                proxyPass = "http://127.0.0.1:${toString config.services.davinci-convert.port}";
                proxyWebsockets = true;
		extraConfig = ''
                  client_max_body_size 3G;  # Allow large file uploads (adjust as needed)
                  client_body_timeout 180s; # Timeout for reading the client request body
                  proxy_read_timeout 180s;  # Timeout for reading a response from the proxied server
                  proxy_send_timeout 180s;  # Timeout for sending a request to the proxied server
                '';
              };
            };
          };

          # User and group for the service
          users.users.davinci-convert = {
            isSystemUser = true;
            group = "davinci-convert";
          };
          users.groups.davinci-convert = {};
	};
      };
    });
}
