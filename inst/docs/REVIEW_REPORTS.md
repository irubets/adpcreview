# Complete ADPC / ADNCA review reports

Load a local dataset, then choose **Dataset review type** on **Review summary**.

## Concentration-time data

Map subject, analyte, visit, nominal time, calculated time and concentration in Setup. Map analysis-relative time and timestamps where available. Choose the scope/filter and timing thresholds, then click **Run review**.

The summary includes the existing timing screens, grouping/profile/subject-ID audits and analysis-population counts. Additional source-quality checks cover missing identifiers, missing/nonfinite values and mapped times, negative concentrations, exact source duplicates, unit consistency and nonstandard analysis flags. Timing-excluded rows are counted and listed. All selected source records remain available to quality checks, including records excluded from timing calculations.

Concentration flags are screens, not adjudicated errors: analysis-relative versus calculated times may differ intentionally, duplicate samples may be legitimate, and supplied units must be consistent. IQR is an optional timing screen. No PC schema, PC variable derivation, protocol windows, BLQ convention, dose history or compartment model is assumed for ADPC.

## Parameter-level NCA data

Choose **NCA parameters (PARAMCD / AVAL)**. Map subject, parameter code and analysis value. Confirm **Unique parameter record key**: subject and parameter are always included; add period, visit, treatment, timepoint, occasion or other fields needed to distinguish legitimate repeats. Suggested defaults use available STUDYID, USUBJID, PARAMCD, APERIOD, AVISIT, ATPT, ADT, ADTM and TRTA fields. These defaults require study review.

Checks include missing identifiers, missing/nonfinite values, repeated selected keys, exact source duplicates, missing/inconsistent units, missing/inconsistent PARAM labels and nonstandard ANLnnFL/NCAnnFL/PKSUMXFL values. APROFILE can be a label and is not interpreted as a binary flag. Populated flags other than Y/N are screened; blanks are allowed and the study specification remains authoritative.

Parameter mode retains all filtered records, provides an inventory and complete listing, and hides concentration-only navigation after review. Concentration plots and NCA recalculation do not apply to these rows. No universal parameter ranges, expected inventory, missing whole profiles or eligibility conclusions are inferred. Missing expected parameters require an authoritative study specification and are not assessed by this implementation.

## Read findings and save reports

Click a linked summary item to open its expandable table below. Tables retain complete available analysis-profile context and highlight triggering rows in amber. Unit/label conflicts mark all involved candidates for review without assuming which value is correct. Missing prerequisites are explicitly **Not assessable**, rather than a clean result. Zero findings only describe evaluated rules.

**Save HTML review (.zip)** creates a linked multipage folder with Summary, evaluation rules, committed settings, audits and findings. Unpack the complete ZIP and open `index.html`. Large HTML tables retain every row and paginate the rendered view.

**Save Excel review (.xlsx)** creates a linked Summary/TOC and separate filterable audit/finding sheets with highlighted triggers and frozen headers. Concentration mode also includes Plot data with NRRLT/ARRLT/MRRLT and AVAL for native Excel scatter plots. Parameter mode provides the complete parameter listing instead.

By default reports save beside the loaded local source file. Existing files receive numbered suffixes. Clear **Save directly beside the loaded local file** for browser downloads. Keep the app open until saving completes. Reports contain selected source records and should receive the same protection as the input dataset.

Reports and their tables use a snapshot from the last successful **Run review**, including mappings, filter and thresholds. Subsequent input edits do not relabel an old run: run again to apply them. PK-profile and separate PKNCA scopes remain independent; this report does not claim that PKNCA was run or include potentially stale calculations from its separate tab.

The complete lowercase SHA256 of source file bytes is captured at load, checked before/after reading, and printed in reports. Failed loads clear provenance. Filters do not change that fingerprint. Programmatically injected data without a successful file load have an explicit unavailable/reload message.

## Validation

`tests/test_complete_review_reports.R` exercises synthetic concentration and parameter reviews, exclusion accounting, duplicate record attribution, committed settings, HTML bundle links, actual Excel content and nonoverwriting local saves. Existing tests cover AI data access, variable mappings, profile identification and plotting modes. Run all tests from the project root as documented in README. Live AI services are not required.
