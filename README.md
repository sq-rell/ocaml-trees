# ocaml trees

To get the distance between two files: navigate to the src folder and run:
`dune exec ./apply_eight.exe PATH_TO_FILE_1 PATH_TO_FILE_2 FUNCTION_NAME`
with the path to the two files, and the function name. For example, if the desired file looks like 
`let add x y = x + y`
then FUNCTION_NAME should be "add".

To generate all the pairwise distances between files that are structured like this:
`some/path/1/actual_file.ml`
`some/path/2/actual_file.ml`
`some/path/4/actual_file.ml`
Go to the helpers folder and open `run_distance.bash`. Set `interest` to the name of the funciton being analyzed. set `HOLD` to the desired destination for the results. set `DATA` to `some/path/`. Set `HW_FILE` to `actual_file.ml`. Set `max_students` to the total number of students. Then run the script with `bash run_distance.bash`

For the files `get-sets.ml`, `get-balls.ml`, `get-centers.ml`, and `get-isolates`, the file they read from is specified on line 15, set this first. Then they can be run with `ocaml get-sets.ml` etc. `get-balls` and `get-centers` have an adjustable radius parameter in the file also. 
