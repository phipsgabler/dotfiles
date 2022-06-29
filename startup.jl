atreplinit() do repl
    # try
        # @eval using Term
        # @eval Term.install_term_repr()
        # @info "Using Term"
    # catch e
        # @warn "Loading Term failed!" exception=(e, catch_backtrace())
    # end

    try
        @eval using Revise
        # @async Revise.wait_steal_repl_backend()
        @info "Using Revise"
    catch e
        @warn "Loading Revise failed!" exception=(e, catch_backtrace())
    end
    
    # try
        # @eval using OhMyREPL
        # @eval enable_autocomplete_brackets(false)
        # @info "Using OhMyREPL"
    # catch e
        # @warn "Loading OhMyREPL failed!" exception=(e, catch_backtrace())
    # end

    @eval using BenchmarkTools
end

@info "Startup done."
