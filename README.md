## How to install
Run `stack install`.

## How to use your version FreeST
It is possible to plug your version of FreeST (for example, for testing/development purposes).
To do so:
- Create a `.tar.gz` package of your FreeST version (you can create them from the original project 
    with `stack sdist`)
- Place your `.tar.gz` file in the `distros` folder
- Change the `archive` value of `extra-deps` in the `stack.yaml` file to your FreeST `.tar.gz` file
    of choosing
- Re-install the server by running `stack install`