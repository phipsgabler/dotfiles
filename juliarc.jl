isinstalled(pkg) = isdir(Pkg.dir(pkg))

if isinstalled("OhMyREPL")
    @eval using OhMyREPL
    enable_autocomplete_brackets(false)
else
    warn("'OhMyREPL' is not installed")
end

# if isinstalled("Revise")
#     @schedule begin
#         sleep(0.1)
#         @eval using Revise
#     end
# else
#     warn("'Revise' is not installed")
# end
