# Contributing to Marc Bernard Tools

Marc Bernard Tools welcomes your suggestions and contributions! Before opening your first issue or pull request, please review our [Code of Conduct](CODE_OF_CONDUCT.md) to understand how our community interacts in an inclusive and respectful manner.

## General Advice

* If you're new and want to help out, see if there are "good first issues" listed [here](../../issues). They should not be complicated to implement but require you to get the project up and running. Or pick something that annoys you. Fix a typo. Improve an error message. Or try something unusual just to see if it works and if it doesn't, open an issue.

* Before starting any significant development, open an issue and propose your solution first. A discussion can save a lot of unnecessary work. It also helps others know that this is being worked on.

* It is in your best interest to keep the commits/PRs as small as possible and solve one thing at a time. The smaller your change is, the easier it is to review and it will be more likely to get accepted.

* Commit often, whenever something is working, and is a step in the right direction do a commit or PR. This way other contributors can see the changes, and it will minimize the risk of merge conflicts.

* If you don't have the time or knowledge to fix the problem yourself, you can still make it move along faster by providing an accurate description or a repo which reproduces the issue.

## Bug Reports

A bug is a _demonstrable problem_ that is caused by the code in the repository. Good bug reports are extremely helpful - thank you!

Guidelines for bug reports:

1. **Use the GitHub issue search** &mdash; check if the issue has already been reported.

2. **Check if the issue has been fixed** &mdash; try to reproduce it using the latest version or development branch in the repository.

3. **Demonstrate the problem** &mdash; provide clear steps that can be reproduced.

A good bug report should not leave others needing to chase you up for more information. Please try to be as detailed as possible in your report. What is your environment? What steps will reproduce the issue? What would you expect to be the outcome? All these details will help to fix any potential bugs.

## Development Guidelines

### Compatibility

Marc Bernard Tools are targeted for SAP Basis 7.31 and higher, so the code should only contain expressions and statements that works on 7.31. abaplint will automatically check every pull request for language syntax that is not available on these releases.

### Linting

Pull requests are checked using [abaplint](https://abaplint.org) and must pass all configured [checks](../../abaplint.json) before they can be merged.

### Pretty Printer

Use pretty printer, keywords upper case + indentation. 

### Prefixing

Variables are prefixed using the standard setting in [abapOpenChecks](http://docs.abapopenchecks.org/checks/69/) naming conventions.

### Internationalization (I18N)

Currently, Marc Bernard Tools support only English language. Neither objects nor text literals are translated. Therefore, all objects shall be set to English as the original language. Text literals in the code shall be maintained in English. 

## Contribution Licensing

Most of our code is distributed under the terms of the [license](LICENSE), and when you contribute code that you wrote to our repositories, you agree that you are contributing under those same terms. In addition, by submitting your contributions you are indicating that you have the right to submit those contributions under those terms.

If you wish to contribute code or documentation *authored by others*, or using the terms of any other license, please indicate that clearly in your pull request so that the project team can discuss the situation with you.
