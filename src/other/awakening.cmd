@ECHO off

SetLocal EnableDelayedExpansion

rem This script is used to run the scala implementation of the
rem solo AI for Labyrinth: The Awakening

rem Set the current working directory to the directory where this script is running.
rem This is important so that all of our relative path references work correctly.
pushd %~dp0

set CLASSPATH=lib\awakening_2.11-2.15.jar;lib\optparse_2.11-2.2.jar;lib\scala-library-2.11.0.jar;^
lib\scala-reflect-2.11.0.jar;lib\scala-pickling_2.11-0.10.1.jar
java -cp %CLASSPATH%  awakening.LabyrinthAwakening %*

popd
EndLocal
