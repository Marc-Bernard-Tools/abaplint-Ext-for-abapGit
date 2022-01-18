![abap package version](https://img.shields.io/endpoint?url=https://shield.abap.space/version-shield-json/github.com/Marc-Bernard-Tools/ABAP-Lint-Ext-for-abapGit/src/zabap_syntax_message_fixer.prog.abap/c_version&label=version&color=darkgray)

# abaplint Extension for abapGit

View [abaplint](https://abaplint.org/) results and detailed findings directly in [abapGit](https://github.com/abapGit/abapGit). 

Made by [Marc Bernard Tools](https://marcbernardtools.com/) giving back to the [SAP Community](https://community.sap.com/)

NO WARRANTIES, [MIT License](LICENSE)

## The Problem

After committing changes from abapGit to you repository, you have to switch to another tool to view the abaplint results. If the abaplint check fails, the detailed findings are hard to process since they don't link back to you SAP system where you want to fix the issues.

## The Solution

This repository provides an extension for abapGit integrating the abaplint results (as [user exits](https://docs.abapgit.org/ref-exits.html)). A summary of the results is displayed in the abapGit repository view and details are shown on a separate page with a code preview and an option to jump directly to the code in question. 

**TODO screenshots**

## Installation

### Repository

You can install the repository using [abapGit](https://github.com/abapGit/abapGit) creating a new online repository for https://github.com/Marc-Bernard-Tools/ABAP-Lint-Ext-for-abapGit. We recommend to use package `$ABAPLINT-EXT`.

### User Exits

Implement abapGit user exits [`wall_message_repo`](https://docs.abapgit.org/ref-exits.html#wall_message_repo) and [`on_event`](https://docs.abapgit.org/ref-exits.html#on_event) as follows:

```abap
wall_message_repo
```

```abap
on_event
```

## Contributions

All contributions are welcome! Just fork this repo and create a pull request. 

## About

<p>Made with :heart: in Canada</p>
<p>Copyright © 2021 <a href="https://marcbernardtools.com/">Marc Bernard Tools</a></p>
<p>Follow <a href="https://twitter.com/marcfbe">@marcfbe</a> on Twitter</p>
<p><a href="https://marcbernardtools.com/"><img width="160" height="65" src="https://marcbernardtools.com/info/MBT_Logo_640x250_on_Gray.png" alt="MBT Logo"></a></p>
