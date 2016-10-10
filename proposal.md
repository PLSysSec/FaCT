# It’s About Time

- Sunjay Cauligi
- Brian Johannesmeyer
- Ariana Mirian
- Gary Soeller

# Short abstract describing the project
We propose taking a language based approach to constant-time code compilation, designing a new domain-specific language called ConstanC. We will apply intermediate representation (IR) transformations to convert non-constant time code representation to constant time where necessary, alleviating the burden placed on the developer and maintainer. We will verify our IR transformation on real security critical code (e.g. AES\*) using ct-verif. Finally, we will design and qualitatively analyze various front-ends for our transformation based on what is realistic and usable.

\*Implementation function subject to change; possible functions to implement: AES, RSA_padding_check_PKCS1_type_2 (from OpenSSL), SVG filter

# What problem are you trying to solve?
Timing side channel attacks have become a serious problem for functions computing on sensitive information. Code depending on sensitive information (e.g. a cryptographic key) may be leveraged by an attacker to infer its value via a shared execution platform or by interacting with the implementation directly through a network.

Some examples are AES key recovery [AES], OpenSSL key recovery [OpenSSL], SSL/TLS data recovery [SSL], and data recovery from memory access patterns. These vulnerabilities have massive privacy implications. [LIO] is a system that addresses these issues in an information flow control system and is able to prevent covert timing channels. [CT-VERIF] is a tool designed to verify whether or not a function runs in constant time.

# How or why are existing solutions not addressing this problem?
CT-Verif verifies whether something is constant-time, but does not actually produce constant-time code from non-constant-time code.

# How are you thinking about solving this problem?
Our plan for this quarter is to start by designing the AST structure for ConstanC. The design of our AST will be guided by our reference implementation as mentioned above, starting with choosing which language features to implement as we need them for our reference implementation. We will also write a code generator, implemented in OCaml, that will take our AST and output LLVM IR. We target IR and not assembly/machine code currently, as we plan on tackling architecture-specific issues (i.e. variable-time instructions) at a later stage.

Once we have completed our reference implementation, we will draft a syntax based on our AST and qualitatively (and likely fairly informally) assess its readability as compared to an equivalent implementation written in C. To show equivalence, we would verify both implementations using CT-Verif to ensure both versions truly run in constant time.

# What are your long term goals? (we added this question) 
We would like to note that the long term goals of this project are beyond the scope for this quarter. Everything listed in the following section is to be considered goals for after this class. 

After implementing the AES\* implementation with our ConstanC AST and showing that it is viable, we hope to expand this project to be a much more general implementation. In the end, we hope to design and create a DSL for expressing constant time function implementations and also build an LLVM backend to compile ConstanC to object files. This would allow more function implementations in a variety of languages, with the hope that this flexibility will convince developers and researchers to use this system. Along with these two goals, we plan to formally verify the compiler and also create a user study that can be used to improve the language syntax and semantics (and in the process quantitatively assess the readability and usage of the language). We would like to add the notion of secret and public data and allow our system to handle this difference. Finally, this could be optimized to specific microarchitectures, but for now we will take a conservative approach.

# References
- [AES] David Gullasch, Endre Bangerter, and Stephan Krenn. Cache games - bringing access-based cache attacks on AES to practice. In 32nd IEEE Symposium on Security and Privacy, S&P 2011, 22-25 May 2011, Berkeley, California, USA, pages 490–505. IEEE Computer Society, 2011.
- [OpenSSL] David Brumley and Dan Boneh. Remote timing attacks are practical. Computer Networks, 48(5):701–716, 2005.
- [SSL] Serge Vaudenay. Security flaws induced by CBC padding - applications to SSL, IPSEC, WTLS ... In Lars R. Knudsen, editor, EUROCRYPT 2002, volume 2332 of LNCS, pages 534–546. Springer, Heidelberg, April / May 2002.
- [LIO] Deian Stefan and Alejandro Russo and Pablo Buiras and Amit Levy and John C. Mitchell and David Mazieres. Addressing Covert Termination and Timing Channels in Concurrent Information Flow Systems, International Conference on Functional Programming (ICFP), September 2012, ACM SIGPLAN
- [CT-VERIF] Jose Bacelar Almeida and Manuel Barbosa and Gilles Barthe and Francois Dupressoir and Michael Emmi. Verifying Constant-Time Implementations, 25th USENIX Security Symposium (USENIX Security 16), August 2016, USENIX Association
