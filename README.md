# Azul-Game

This project simulates an Azul game using the Prolog programming language. 

## Starting
To use the project, clone it or download it to your local computer.


### Requirements 📋
It is necessary to have `swi-prolog`, to use the project rules for run the game and the test suit. 

To install the depencies run:
```
make install
```


### Execution ▶️

To execute the project, just open the console from the root location of the project and execute:
```
make run
```
The previous rule run a standar game of 4 players and 9 factories. To modify this values use change the Makefile parameters:
```
make run players=3 fac=5
```

After that you could see the description of th game events in `log.log` file by default but this behaivior is modifiable use the `file` variable. If you need more details about the game events change the variable `level` to debug:
```
make run file=myfile.log level=debug
```

Additionally to see how to run the test suit use:
```
make help
```


## Authors ✒️

* **Lazaro Raul Iglesias Vera** -- [stdevRulo](https://github.com/stdevRulo)
* **Miguel Tenorio Potrony** - [stdevAntiD2ta](https://github.com/stdevAntiD2ta)

## License 📄

This project is under the License (MIT License) - see the file [LICENSE.md](LICENSE.md) for details.
