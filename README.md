# NCF-thesis
Code and writeup of my Undergraduate thesis "EXHIBITION TO INSTITUTION," written in partial fulfillment of my BA from New College of Florida in Spring of 2026.

Read the full thesis here: https://digitalcommons.ncf.edu/theses_etds/7012/

View the slides used in my thesis defense/Baccalaureate exam: https://canva.link/5vtulshugvi9dk5
- Note that the slides may feel incomplete without the actual presentation and discussion

**Abstract**

This thesis explores the relationship between acquisitions at The Museum of Modern Art
(MoMA), and exhibitions at MoMA PS1 (formerly the independent exhibition space Project
Studio 1, stationed in Long Island City’s repurposed schoolhouse Public School 1) . Particular
attention was given to how the influences might change before and after the 1994-2000 merger
of the two institutions. The thesis also addresses the challenges involved with collecting
ephemeral or conceptual artworks. Binary and cumulative logistic regression were used to assess
the significance of artist and exhibition features on whether an artist who exhibited at PS1 had a
work that is acquired into the permanent collection of MoMA, and how quickly that acquisition
occured. Results of the regression models fitted in this project suggest that the acquisitions
coming from PS1 exhibitions are most clearly linked to artists who are already established, as
measured by previous acquisitions, contradicting the idea that PS1 exclusively deals with artists
for whom the traditional museum space is inaccessible. The findings also indicate that the less
established artists whose practice involved the use or creation of documenting material seemed
to have had more works acquired by MoMA. The data used in this project is MoMA PS1’s
exhibition history ranging May 1971 - October 2025, which includes some proto-PS1 exhibitions
that were produced by the founder of PS1, and a public MoMA-published database containing
information on all works in their permanent collection as of June 2025. The PS1 exhibition
history data was obtained from www.moma.org/calendar/exhibitions/history

**File Descriptions**

- 01Webscraping: Code used to "semi-scrape" PS1's exhibition history from MoMA's website. "Semi-scrape" refers to the need to compile all page HTMLs into a series of .txt files and import those into R before converting them to a vector of HTML files that I could use traditional webscraping methods on. This is different than regular webscraping, which is able to 'grab' the HTML of a page just given a url, which was not possible in this case. There are three separate .txt files for each scraping workflow (different workflow needed for exhibitions by multiple vs single artists), and this file demonstrates the workflow for one of each method. The .txt files are not included in this repository.
- 02ExhibitionDepts: The "genre" of exhibition was a desirable predictor in this project, and for the exhibitions that had a description available, I tried multiple methods to automate the process of figuring out the "genre" using the text in the description. The three methods in this file were seeded Latent Dirichlet Allocation (LDA), and two methods that worked like a keyword search. For accuracy, all descriptions were assigned by hand in the end.
- 03ArtistGender: Gender was unknown for a large majority of the artists at PS1, and this file outlines a simple method of estimating a gender score that would give us the confidence that an artist was Male or Female. This is the only file in the project that utilized GenAI, which was used only to sort names, and not to produce any code or writing that went into the actual thesis. 
- 04Regression: Cleaning of MoMA's self-published collection database, which was then used to create regression response variables. This file also contains all modeling done in this project, from variable selection to model fitting (logistic regression, cumulative link logistic regression) to simple performance metrics. This file was written at the same time as EDA was being performed to help guide some of the regression preparation decisions being made; EDA is not included in this repository. 
