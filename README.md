A demonstration of how legacy COBOL code can be modernized into a highly maintainable form.

This repo takes a COBOL program from https://github.com/fgregg/tax_extension and showcases different stages that can be taken to safely refactor it into modern Java code by a skilled software craftsman assisted by AI.  Several folders exist:
- [Documentation](Documentation) - Detailed AI-assisted documentation on key aspects of the program.
- [OriginalCobolCode](OriginalCobolCode) - The original COBOL program code.
- [AnnotatedCobolCode](AnnotatedCobolCode) - The original COBOL program code but annotated with additional comments by AI to assist in program understanding and verification.
- [InitialJavaModernization](InitialJavaModernization) - An initial AI-assisted Java modernization of the COBOL program, using agentic AI with supervision.  Useful for initial verification alongside automated test frameworks.
- [ImprovedJavaModernization](ImprovedJavaModernization) - An improved Java modernization of the COBOL program, with a skilled software craftsman scrutinizing the design and maintainability of the code, leveraging AI to accelerate improvements.
