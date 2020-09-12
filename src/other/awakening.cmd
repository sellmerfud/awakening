@ECHO off

SetLocal EnableDelayedExpansion

rem This script is used to run the scala implementation of the
rem solo AI for Labyrinth: The Awakening

rem Set the current working directory to the directory where this script is running.
rem This is important so that all of our relative path references work correctly.
pushd %~dp0

set CLASSPATH=lib\awakening_2.11-4.6.jar;lib\optparse_2.11-2.2.jar;lib\scala-library-2.11.0.jar;^
lib\scala-parser-combinators_2.11-1.1.2.jar
java -cp %CLASSPATH%  awakening.LabyrinthAwakening %*

popd
EndLocal
