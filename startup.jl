# if isinstalled("OhMyREPL")
#     @eval using OhMyREPL
#     enable_autocomplete_brackets(false)
# else
#     warn("'OhMyREPL' is not installed")
# end

atreplinit() do repl
    try
        @eval using Revise
        @async Revise.wait_steal_repl_backend()
    catch
    end
end

@info "Startup done."
