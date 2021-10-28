[![doi](https://img.shields.io/badge/doi-10.1002%2Fhyp.14403-brightgreen)][doi]

# How did the suspended sediment load change in the North Caucasus during the Anthropocene?

This repository contains the main processing steps to explore suspended sediment load (SSL) temporal change in the Terek basin (North Caucasus) during the Anthropocene. It is meant to accompany a journal article ([*Tsyplenkov et al., 2021*][doi]). This study is part of the ongoing Russian Science Foundation project No. 19-17-00181: *«Quantitative assessment of the slope sediment flux and its changes in the Holocene for the Caucasus mountain rivers.»*

For a deep introduction to the study, please refer to:
>Tsyplenkov AS, Golosov VN, Belyakova PA. 2021. How did the suspended sediment load change in the North Caucasus during the Anthropocene? Hydrological Processes 35 (10): 1–20 DOI: 10.1002/hyp.14403


***

Full text of the paper is available [here][doi].

To replicate main results, follow the instructions in `R` directory and, of course, feel free to explore in depth the chunks of code or rise an issue, or write me a email.

Follow us on Twitter: [@atsyplen][ats].

[doi]: https://doi.org/10.1002/hyp.14403
[ats]: https://twitter.com/atsyplen

***

## REPLICATION. HOW TO
1. Fork this repository or [unzip the archive][arch].
2. Using RStudio open "sediment-caucasus-anthropocene.Rproj" file in the main project directory.
3. Run the `R/00_prepare-r-session.R` file. 
4. Now you can run other files in `R` directory.

## LOGIC OF THE PROCESS
The whole process is split into four parts, which is reflected in the structure of R scripts. Every `R` file is independent and not related to others. For example, if you want to reproduce only Figure 2 , then run `R/02_figure2.R`.
The names of the scripts are quite indicative, and each script is reasonably commented.

## SEE ALSO
 - [Our other paper, which explores intra-event suspended sediment dynamics in the small glacierized basin in Caucaus mountins (Djankuat)][jss]
 - [For the same Djankuat basin we have also adopted a fingerprinting model to study sediment sources][catena]
 - [Study describing suspended sediment spatial patterns in the Caucasus mountains][piahs]
 - [Paper (Lizaga et al., 2020) describing the FingerPro model][fingerpro]

[catena]: https://doi.org/10.1016/j.catena.2021.105285
[jss]: https://doi.org/10.1007/s11368-020-02633-z
[piahs]: https://doi.org/10.5194/piahs-381-87-2019
[arch]: https://github.com/atsyplenkov/sediment-caucasus-anthropocene/archive/refs/heads/master.zip
