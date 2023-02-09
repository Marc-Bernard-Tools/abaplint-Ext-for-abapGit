![Version](https://img.shields.io/endpoint?url=https://shield.abap.space/version-shield-json/github/Marc-Bernard-Tools/ABAP-Lint-Ext-for-abapGit/src/zif_abaplint_abapgit_ext.intf.abap/c_version&label=Version&color=blue)

[![License](https://img.shields.io/github/license/Marc-Bernard-Tools/ABAP-Lint-Ext-for-abapGit?label=License&color=green)](LICENSE)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg?color=green)](CODE_OF_CONDUCT.md)
[![REUSE Status](https://api.reuse.software/badge/github.com/Marc-Bernard-Tools/ABAP-Lint-Ext-for-abapGit)](https://api.reuse.software/info/github.com/Marc-Bernard-Tools/ABAP-Lint-Ext-for-abapGit)
[![ClearlyDefined Score](https://img.shields.io/clearlydefined/score/git/github/marc-bernard-tools/abap-lint-ext-for-abapgit/e7046746c18809e14f129c1644b2c2bf537e58ac?label=ClearlyDefined%20Score)](https://clearlydefined.io/definitions/git/github/marc-bernard-tools/abap-lint-ext-for-abapgit/e7046746c18809e14f129c1644b2c2bf537e58ac)

# abaplint Extension for abapGit

View [abaplint](https://abaplint.org/) results and detailed findings directly in [abapGit](https://github.com/abapGit/abapGit). 

Made by [Marc Bernard Tools](https://marcbernardtools.com/) giving back to the [SAP Community](https://community.sap.com/)

NO WARRANTIES, [MIT License](LICENSE)

## The Problem

After committing changes from abapGit to your repository and running abaplint there, you have to switch to GitHub or another tool to view the abaplint results. If the abaplint check fails, the detailed findings are hard to process since they don't link back to your SAP system where you want to fix the issues.

![github2](img/github_2.png)

Note: VSCode with [abaplint extension](https://marketplace.visualstudio.com/items?itemName=larshp.vscode-abaplint) is an exception showing lint results within your ABAP code. But then you are not using abapGit in the first place.

## The Solution

This repository provides an extension for abapGit integrating the abaplint results (as [user exits](https://docs.abapgit.org/ref-exits.html)). A summary of the results is displayed in the abapGit repository view and details are shown on a separate page with a code preview and an option to jump directly to the code in question. 

### Repository View

Under the name of the repository, the status and summary of the last abaplint check will be displayed. 

![check1](img/check_success.png)

![check2](img/check_in_progress.png)

![check3](img/check_failure.png)

You can click on the status icon to open the corresponding page on GitHub. If you click on the result summary, the detailed issues will be shown.

### Issue View

The issues are listed similarly to syntax and ATC checks in abapGit. For each finding, the view shows the object type, name, and line number. Below, it prints the abaplint message. A link on the error code opens the corresponding definition on [rules.abaplint.org](https://rules.abaplint.org/). 

![findings1](img/findings_error_1.png)

Optionally, you can sort the results by object, location, or error code. You can also hide the source code preview.

![findings1](img/findings_error_2.png)

## Installation

### Prerequisites

abaplint needs to be installed and given access to your GitHub repository.

1. Install abaplint

   https://github.com/apps/abaplint/installations/new

2. Give abaplint access to your repos 

   https://github.com/settings/installations

3. Add abaplint.json to your repo

   Get latest default from https://playground.abaplint.org (2nd tab)

### Repository

You can install the repository using [abapGit](https://github.com/abapGit/abapGit) creating a new online repository for https://github.com/Marc-Bernard-Tools/ABAP-Lint-Ext-for-abapGit. We recommend to use package `$ABAPLINT-EXT`.

### User Exits

Implement abapGit [user exits](https://docs.abapgit.org/ref-exits.html) [`wall_message_repo`](https://docs.abapgit.org/ref-exits.html#wall_message_repo) and [`on_event`](https://docs.abapgit.org/ref-exits.html#on_event) as follows:

```abap
  METHOD zif_abapgit_exit~wall_message_repo.

    zcl_abaplint_abapgit_ext_exit=>get_instance( )->wall_message_repo(
      is_repo_meta = is_repo_meta
      ii_html      = ii_html ).

  ENDMETHOD.
```

```abap
  METHOD zif_abapgit_exit~on_event.

    IF rs_handled IS INITIAL.
      rs_handled = zcl_abaplint_abapgit_ext_exit=>get_instance( )->on_event( ii_event ).
    ENDIF.

  ENDMETHOD.
```

### SSL and Certificates

The abaplint status is retrieved via `https://api.github.com/`. Therefore, a proper SSL configuration and certificates for `api.github.com` are required. See [SSL setup](https://docs.abapgit.org/guide-ssl-setup.html) for details.

## Contributions

All contributions are welcome! Read our [Contribution Guidelines](CONTRIBUTING.md), fork this repo, and create a pull request.

## About

Made with :heart: in Canada

Copyright 2021 Marc Bernard <https://marcbernardtools.com/>

Follow [@marcfbe](https://twitter.com/marcfbe) on Twitter

<p><a href="https://marcbernardtools.com/"><img width="160" height="65" src="https://marcbernardtools.com/info/MBT_Logo_640x250_on_Gray.png" alt="MBT Logo"></a></p>
