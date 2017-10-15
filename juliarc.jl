let pkgs = Pkg.installed()
    if haskey(pkgs, "OhMyREPL")
        using OhMyREPL
    else
        warn("'OhMyREPL' is not installed")
    end

    if haskey(pkgs, "Revise")
        @schedule begin
            sleep(0.1)
            @eval using Revise
        end
    else
        warn("'Revise' is not installed")
    end
end
