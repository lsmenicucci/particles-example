mkdir -p dist


# get test_case number from command line
test_case=$1 
test_case_src="test_case_$test_case.f90"
test_case_out="./dist/test_case_$test_case.out"

echo "Building test case $test_case_src => $test_case_out"

# build 
gfortran src/*.f90 $test_case_src -o $test_case_out -J ./dist
gfortran src/*.f90 $test_case_src -o $test_case_out -J ./dist
gfortran src/*.f90 $test_case_src -o $test_case_out -J ./dist
