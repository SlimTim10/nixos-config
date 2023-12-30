{
  pkgs,
  osConfig,
  ...
}:

let
  # Syncthing status for xmobar
  syncthingApiKey = "$(cat ${osConfig.age.secrets."syncthingApiKey".path})";
  xmobar-syncthing-status = pkgs.writeShellScriptBin "xmobar-syncthing-status" ''
    status="$(curl --silent -X GET -H "X-API-Key: ${syncthingApiKey}" http://localhost:8384/rest/db/status?folder=Sync | jq -r '.state')"
    echo "Syncthing: $status"
  '';
in {
  home.file.".xmobarrc" = {
    source = ./.xmobarrc;
  };

  home.packages = with pkgs; [
    xmobar # status bar for xmonad
    jq # command-line JSON processor
    xmobar-syncthing-status # uses jq
  ];
}
