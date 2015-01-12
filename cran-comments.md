This is a resubmission of shiny 0.11 with update license information.

As per our communication, all copyright holders listed in the sources are listed as "cph" in Authors@R. Where the copyright holders are natural persons, they are also listed as "ctb". Where the copyright holders are not natural persons, I have taken the following approach:

* jQuery: The copyright holders are "jQuery Foundation and other contributors". I have listed "jQuery Foundation" as "cph", and "jQuery contributors" as "ctb" and "cph". (According to jQuery's AUTHORS.txt file, there are hundreds of people who have contributed to jQuery, so I have not listed them each individually.)
* Bootstrap: The copyright holder is Twitter, Inc. I have listed "Twitter, Inc", as "cph", and "Bootstrap contributors" as "ctb". The Bootstrap project page also names two creators of the project, and so I have listed them as "ctb". (According to Bootstrap's commit history, there are hundreds of others who have contributed to Bootstrap, so I have not listed them each individually.)
* es5-shim: The copyright holders are "Kristopher Michael Kowal and contributors". I have listed "Kristopher Michael Kowal" as "ctb" and "cph", and "es5-shim contributors" as "ctb" and "cph". (According to es5-shim's CONTRIBUTORS.md file, there are dozens of others who have contributed to es5-shim, so I have not listed them each individually.)
* DataTables: The copyright holder is "SpryMedia Limited". Because no natural person is listed in the documentation for DataTables, I have "SpryMedia Limited" as "ctb" and "cph".


The following NOTEs come up in R CMD check:

* checking installed package size ... NOTE
  installed size is  6.2Mb
  sub-directories of 1Mb or more:
    www   4.4Mb

  This is because there are many web resources in the that directory which are necessary for Shiny to function.
