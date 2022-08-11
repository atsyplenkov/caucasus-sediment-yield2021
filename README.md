[![doi](https://img.shields.io/badge/doi-10.3390%2Fw13223173-brightgreen)][doi]

# Factors Controlling Contemporary Suspended Sediment Yield in the Caucasus Region 

This repository contains the main processing steps to spatial variation of mean annual area-specific sediment yield (SSY, t km<sup>−2</sup> yr<sup>−1</sup>). It is meant to accompany a journal article ([*Tsyplenkov et al., 2021*][doi]). This study is part of the ongoing Russian Science Foundation project No. 19-17-00181: *«Quantitative assessment of the slope sediment flux and its changes in the Holocene for the Caucasus mountain rivers.»*

For a deep introduction to the study, please refer to:
>Golosov V, Tsyplenkov A. 2021. Factors Controlling Contemporary Suspended Sediment Yield in the Caucasus Region. Water 13 : 3173. DOI: 10.3390/w13223173


***

Full text of the paper is available [here][doi].

To replicate main results, follow the instructions in `R` directory and, of course, feel free to explore in depth the chunks of code or rise an issue, or write me a email.

Follow us on Twitter: [@atsyplen][ats].

[doi]: https://doi.org/10.3390/w13223173
[ats]: https://twitter.com/atsyplen

***

## REPLICATION. HOW TO
1. Fork this repository or [unzip the archive][arch].
2. Using RStudio open "caucasus-sediment-yield2021.Rproj" file in the main project directory.
3. Run the `R/00_prepare-r-session.R` file. 
4. Now you can run other files in `R` directory.

## LOGIC OF THE PROCESS
The whole process is split into eight parts, which is reflected in the structure of R scripts. Every `R` file is independent and not related to others. For example, if you want to reproduce only correlation analysis , then run `R/03_cor-analysis.R`.
The names of the scripts are quite indicative, and each script is reasonably commented.

## SEE ALSO
 - [Our other paper, which explores intra-event suspended sediment dynamics in the small glacierized basin in Caucaus mountins (Djankuat)][jss]
 - [For the same Djankuat basin we have also adopted a fingerprinting model to study sediment sources][catena]
 - [Study describing suspended sediment spatial patterns in the Caucasus mountains][piahs]

[catena]: https://doi.org/10.1016/j.catena.2021.105285
[jss]: https://doi.org/10.1007/s11368-020-02633-z
[piahs]: https://doi.org/10.5194/piahs-381-87-2019
[arch]: https://github.com/atsyplenkov/sediment-caucasus-anthropocene/archive/refs/heads/master.zip
