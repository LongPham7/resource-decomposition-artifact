This artifact demonstrates a hybrid-resource-analysis technique called resource
decomposition.

To build a Docker image, in the root director, run
```bash
docker build -t resource-decomposition .
```
Note that we need a period at the end of the command to indicate the current
(i.e., root) directory contains a Dockerfile.

To run the Docker image, run
```bash
docker run -it --rm resource-decomposition
```

The artifact is described in a document README.pdf, whose LaTex source files are
stored in the directory `README`. To build the document, go to the directory
`README` and compile the LaTex source files. For example, if you already have
`latexmk` on your machine, run
```bash
latexmk --pdf RAEDME
```
