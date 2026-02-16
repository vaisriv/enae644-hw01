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
        # python helper
        {
            name = "py";
            category = "[submission]";
            help = "run submission python script";
            command =
                # bash
                ''
                    CYAN="\e[0;36m"
                    NC="\e[0m"

                    cd $(git rev-parse --show-toplevel)

                    echo -e -n "$CYAN"
                    echo -e "running python script for $(basename $(pwd)):$NC"
                    python ./submission.py
                '';
        }
    ];

    packages = with pkgs; [
        # python
        (python3.withPackages (ps:
            with ps; [
                # python packages here
                matplotlib
                numpy
                scipy
                cartopy
            ]))
    ];
}
