[![Build Status](https://travis-ci.org/wol4aravio/KaimereApp.svg?branch=master&pony=17)](https://travis-ci.org/wol4aravio/KaimereApp.svg?branch=master)

# Basic Description
Application of Kaimere project to different optimization tasks.
Currently supporty optimization of Simulink models using different optimization techniques (SimulinkOptimizer app).

# Installation
After you have downloaded sources you need to call `sbt assembly`. 

Do not forget to provide the following flag: `-Djava.library.path="..."`

You need to specify path to 
* `maci64` folder of Matlab (for Os X),
* `glnxa64` folder of Matlab (for *nix),
* `win64` folder of Matlab (for Windows).

# Instructions
Now you can use provided app. Here is the example of calling script:
```bash
java -jar -Djava.library.path=<YOUR PATH> Kaimere.jar
     --matlab-engine <PATH TO MATLAB ENGINE.JAR>
     --simulink-model-slx <PATH TO SIMULINK .SLX FILE>
     --simulink-model-json <PATH TO OPTIMIZATION TASK .JSON FILE>
     --optimization-tool <PATH TO .JSON CONFIG FILE OF OPTIMIZATION ALGORITHM>
     --area "a:-100:100" "b:-100:100" ...
     --time 900
```
