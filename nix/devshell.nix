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
    ];
}
