{
    pkgs,
    perSystem,
    ...
}:
perSystem.devshell.mkShell {
    name = "enae644-hw01 devshell";
    motd = ''
        {141}📚 hw{reset} devshell
        $(type -p menu &>/dev/null && menu)
    '';

    commands = [
    ];

    packages = with pkgs; [
        # python
        (python3.withPackages (ps:
            with ps; [
                # python packages here
                matplotlib
                numpy
            ]))

        # haskell
        # (ghc.withPackages (hsPkgs:
        #     with hsPkgs; [
        #         pqueue
        #     ]))

        # cabal2nix
        ghc
        cabal-install
        haskell-language-server
    ];
}
