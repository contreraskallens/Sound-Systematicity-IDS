using DataFrames
using DataFramesMeta
using RCall
using CSV
using Statistics
using LinearAlgebra
using Images
using JuliaDB
using StatsBase
R"require(stringdist)"

function get_distance_matrix(words::Vector{String})::Array{Float64, 2}
    @rput words
    # R"matrix <- stringdist::stringdistmatrix(words, words, weight = c(1, 1, 1, 0.01))"
    R"matrix <- stringdist::stringdistmatrix(words, words, method = \"lv\")"
    @rget matrix
end

function get_mean_distance(distances::Array{Float64, 2}, classes::Vector{String})
    distances[diagind(distances)] .= NaN
    Thing_vector = classes .== "Thing"
    Action_vector = classes .== "Action"
    Thing_matrix = distances[:, Thing_vector]
    Action_matrix = distances[:, Action_vector]
    Thing_distances = meanfinite(Thing_matrix, 2) |> vec
    Action_distances = meanfinite(Action_matrix, 2) |> vec
    typicality = Action_distances .- Thing_distances
    mean_distances = JuliaDB.table((class = classes, meanAction = Action_distances, meanThing = Thing_distances, typicality = typicality))
    # mean_distances = hcat(classes, Thing_distances, Action_distances, typicality)
    # convert(Array{Union{Float64, String}, 2}, mean_distances)
end

function get_typicality_stats(lang_frame::DataFrame, distance_matrix::Array{Float64, 2}, language::String, permutation = nothing)
    if permutation != nothing
        lang_frame[:ontologicalCategory] = StatsBase.sample(lang_frame[:ontologicalCategory], nrow(lang_frame), replace = false)
        if permutation % 1000 == 0
          display(permutation)
          print("\n")
        end
    end
    distances = get_mean_distance(distance_matrix, lang_frame[:ontologicalCategory])
    distances = JuliaDB.summarize((mean, std), distances, :class, stack = true)
    if permutation != nothing
        distances = pushcol(distances, :permutation => repeat([permutation], length(distances)))
    end
    distances = pushcol(distances, :language => repeat([language], length(distances)))
    CSV.write("mc_results.csv", distances, append = true)
end

function get_mc_distances(language::String, iterations::Int64, all_data::DataFrame = language_data)
    display(language)
    print("\n")
    lang_frame = @where(all_data, :language .== language)
    distance_matrix = get_distance_matrix(lang_frame[:phon])
    for iteration in 1:iterations
#     distances = mapreduce(x -> get_typicality_stats(lang_frame, distance_matrix, x), JuliaDB.merge, 1:iterations)
      get_typicality_stats(lang_frame, distance_matrix, language, iteration)
    end
end

function get_all_distances(iterations)
  language_data = CSV.File("Data/Processed/allPhonFormsConcepticon.csv") |> DataFrame |> disallowmissing!
  for language in sort(unique(language_data[:language]))
      mc_distances = get_mc_distances(language, iterations, language_data)
  end
end

get_all_distances(1000)

# mc_distances = mapreduce(x -> get_mc_distances(x, 100000), JuliaDB.merge, unique(language_data[:language]))
# CSV.write("mc_results.csv", mc_distances)
