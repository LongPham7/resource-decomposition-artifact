FROM ubuntu:22.04
RUN apt update && apt -y upgrade

# Install Python and pip
RUN apt install -y python3.10 pip

# Install OCaml 4.06.0
RUN apt install -y opam
RUN opam init --comp 4.06.0 --disable-sandboxing -y
RUN eval $(opam env)

# Install wget (to be used by subsequent commands in this Dockerfile) and vim
# (so that the user can edit files inside the Docker container)
RUN apt install -y wget

# Install OCaml libraries. First, I enable the installation of archived OCaml
# libraries because the core library v0.11.3, which is required by RaML, is
# archived as of June 2025. This means the main repository of OCaml libraries
# used by opam no longer has this version of the core library. To workaround
# this issue, I resort to this GitHub repository:
# https://github.com/ocaml/opam-repository-archive.
RUN opam repo add archive git+https://github.com/ocaml/opam-repository-archive
RUN opam install -y ocamlbuild.0.14.0 core.v0.11.3

# Install the library liblapacke-dev, which is necessary for the build of RaML.
# This library is probably used by the LP solver CLP used inside RaML.
RUN apt install -y liblapacke-dev

# Install CLP
WORKDIR /home/clp
RUN wget https://github.com/coin-or/Clp/releases/download/releases%2F1.17.9/Clp-releases.1.17.9-x86_64-ubuntu22-gcc1140.tar.gz
RUN tar xvzf Clp-releases.1.17.9-x86_64-ubuntu22-gcc1140.tar.gz

# Install the Python-Stan binding and other Python libraries
RUN pip install pystan numpy matplotlib joblib networkx

# Install OCaml 4.14.0. This version of OCaml is used for running OCaml
# benchmark code.
RUN opam switch create 4.14.0
RUN opam switch 4.14.0
RUN eval $(opam env)

# Install OCaml libraries for OCaml 4.14.0
RUN opam install -y core yojson core_unix

# Copy RaML from a local machine to the Docker container
ADD ./raml /home/raml

# Build RaML
WORKDIR /home/raml
RUN ./configure --with-coin-clp /home/clp
ENV LD_LIBRARY_PATH "/home/clp/lib"
RUN opam switch 4.06.0 && eval $(opam env) && make

# Add RaML's executable to the PATH so that we can call main anywhere in the
# filesystem to run RaML
ENV PATH "$PATH:/home/raml"

# Copy the directory of Python code for running experiments and visualizing
# results from a local machine to the Docker container
ADD ./experiment /home/experiment

# Copy the directory of OCaml benchmark source code from a local machine to the
# Docker container
ADD ./ocaml-benchmarks /home/ocaml-benchmarks

# Switch back to OCaml 4.14.0 from OCaml 4.06.0, which was used to build RaML
RUN opam switch 4.14.0 && eval $(opam env)

# Go to the directory of OCaml benchmark source code
WORKDIR /home/ocaml-benchmarks
ENTRYPOINT ["opam", "exec", "--"]
CMD ["/bin/bash", "--login"]
