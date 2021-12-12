# AlphaRegexPublic

## AlphaRegex : Synthesizer for regular expressions
AlphaRegex is a synthesizer 
that instantly outputs regular expressions from examples
for introductory automata classes,
This work was conducted under the guidance of 
Prof.Hakjoo Oh (http://prl.korea.ac.kr)
in Korea University.

## Paper
AlphaRegex performs over- and under approximations 
of partial regular expressions to accelerate the speed.
For the more details, please refer to 
our GPCE '16 paper ```Synthesizing Regular Expressions from Examples for Introductory Automata Assignments```.
Please cite our paper if you find our work is intriguing.

## How to build
After you clone this project, follow the steps below.
-   Activate the build script: ```chmod +x build```.
-   Complie the whole files using the script: ```./build```.

## How to build with dune:
Simply run ```dune build main.exe```.


## How to use
To use AlphaRegex, you need to provide:
-   Positive examples
-   Negative examples

Then, AlphaRegex quickly derives a regular expression
that accepts all the positive examples and 
rejects all the negative examples.
For example, type ```./main.native -input benchmakrs/no1_start_with_0```,
then AlphaRegex instantly generate the solution ```0(0+1)^*```.
In the ```benchmarks``` directory, there are benchmark examples
that we used for evaluation and you can easily check 
how positive/negative examples should be given to AlphaRegex.

## Contact
AlphaRegex is maintained by Programming Research Laboratory in Korea University
(http://prl.korea.ac.kr).
If you have any questions, feel free to email us: 
hakjoo\_oh@korea.ac.kr, sunbeom\_so@korea.ac.kr 
