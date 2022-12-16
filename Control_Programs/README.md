# Control Programs 

VCQI analyses are run using control programs which define information about the survey and set analysis parameters. Users can download these example control programs and modify them to conduct VCQI analyses of their own. 

- The District 20 RI Demo Control Program accompanies the *Getting Started with VCQI for R Users* document. This control program is configured to analyze data from a simple single-stratum routine immunization (RI) survey in the fictional District 20. The District 20 data can be found in the Demo_Datasets folder in this repository. 
- The User's Guide RI Control Program accompanies the *VCQI R User's Guide* document. This control program is configured to analyze RI survey data from the fictional country of Harmonia. The Harmonia data can be found in the Demo_Datasets folder in this repository. The VCQI R User's Guide provides detailed information on how this example control program can be customized to analyze other routine immunization surveys. 

The globals_for_timeliness_plots script defines parameters for VCQI's vaccination coverage and timeliness charts (VCTCs). To customize the appearance of VCTCs, users may download this file, modify and save it, and edit their control program to provide the filepath for the modified script when defining VCTC_globals_path in Block F. Refer to Chapter 6 of the VCQI R User's Guide for more information on customizing VCTCs. 
