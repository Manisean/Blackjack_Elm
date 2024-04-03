{ pkgs }: {
    deps = [
        pkgs.elmPackages.elm
        pkgs.elmPackages.elm-live
        pkgs.elmPackages.elm-language-server
    ];
}