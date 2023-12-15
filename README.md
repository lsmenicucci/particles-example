A very simples example of a structured numerical project in modern fortran. The example taken is a simple molecular dynamics simulation using the velocity verlet integration scheme. 

### Structure

Source code in `src` folder is modularized and derived by two components `base_test_case` and `io_base`. `base_test_case` depends on an `io_base` implementation since the latter is an abstract type. Test cases are a model for a simulation setup, io is swappable as well as particles interactions and boundary conditions. Only a `io_csv` type is implemented and plotting is configured to work with this output. 

### Test cases

Test cases grow in incremental complexity, demonstrating the versatility of the current setup:
- `test_case_1.f90` initial condition is a set of particles in a circle with radial velocity
- `test_case_2.f90` Box boundary conditions is added
- `test_case_3.f90` Lenard-Jones interaction is added

### Compilation

Because some modules depend on others, it might be necessary to run `./build.sh` twice or many times in order to advance in the dependency tree. A more robust setup would rely on a Makefile and a proper handle of these dependencies, but thats not the focus here.

General build usage:
```bash
./build.sh 2 # build test_case_2.f90
```

Shorthand:
```bash
./build-n-plot.sh 2 # build test_case_2.f90, execute it and plot results
```
