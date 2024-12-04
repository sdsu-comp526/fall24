# Syllabus

## Logistics

*Course name*: Comp-526

*Course term*: Fall 2024

*Class time*: **M-W-F: 9-9:50 am**

*Mode of delivery*: **in person**

*Location*: **NE 172**


### Instructor: [Valeria Barra](https://valeriabarra.org)

Pronouns: (she/her/hers)

Email: vbarra @ sdsu . edu

Office location: announced on the course [Canvas page](https://sdsu.instructure.com/courses/161092)


**Office Hours**: announced on the course [Canvas page](https://sdsu.instructure.com/courses/161092) or by appointment.

:::{tip}
Office hours are an important time for asking questions, solving problems, discussing broader academic and career strategies, and providing feedback so I can make the class serve your needs and those of people with similar experiences and interests.
:::

## Overview

Scientific computing and mathematical modeling (both deterministic and stochastic) are fundamental tools for the solution of problems arising in the study of complex systems, whether originating from the physical, chemical, or biological sciences, or of an economic and social nature.

_Scientific computing_ is a broad term that describes the use of computers for scientific, medical, and engineering applications. This definition is often application-focused or domain-driven.

On the other hand, _Numerical Analysis_, is used to solve problems in analysis (i.e., with real numbers) by numerical, rather than symbolic means. Some examples:

- Solving linear and nonlinear equations
- Computing values of definite integrals
- Solving differential equations

This can be seen as a problem-focused description.

_Numerical (or Computational) Methods_ are algorithms (methods) for solving mathematical problems in the
service of applications. Some examples:

- The method of conjugate gradients for linear systems
- Gaussian quadrature for computing integrals
- The Runge-Kutta method for initial value problems

This is an algorithm-focused description.
In reality, this area encompasses all three: based on an application, we formulate some mathematical problems and find an algorithm for
it to solve it using computers.


## Organization and course design

We will start by giving an introduction to version control and reproducibility, which are key aspects of modern computational sciences. Then we will introduce the Linux filesystem and some basic shell commands.

We'll proceed with the evaluation of functions and introduce the concepts of conditioning and stability, which are applicable to every topic we encounter.

Then we'll explore rootfinding, our first infinite algorithm, in which we'll learn about convergence classes and the fundamental challenge of writing a function that is correct for all well-typed inputs.

We'll move to the concepts of stability and backward stability.

Next up will be an introduction to linear algebra, interpolation, and then differentiation.

We'll move on to integration and finally numerical solution of differential equations.

Although we'll continue with new content, during the semester we will have two separate short modules on compiled languages programming: C and Fortran.

Towards the end of the semester, for your final projects, you can then form small teams of like interest and work on an original study (numerical experiments and interpretation, comparisons, etc.) or on contribution to be shared with the community. Studies and contributions can take many forms.

## Student Learning Outcomes

Upon completing this course, students will be able to

1. contribute to collaborative software with the use of version control systems, such as `git`
2. formulate problems in science and engineering in terms of computational methods
3. evaluate the accuracy and performance of algorithms
4. diagnose ill-conditioned problem formulations and unstable algorithms
5. develop effective numerical software, taking into account stability, accuracy, and cost
6. communicate about the above using figures, numerical experiments, writing, and presentation
7. search for and understand relevant literature and documentation
8. write programs in Julia, C, and Fortran

### Expectations

1. Enter with a growth mindset, practice adaptive coping, and nurture your intrinsic motivation
2. Attend class (in-person) and participate in discussions
3. Make an honest attempt at activities, projects, etc.
4. Interact with the class notebooks and read reference material
5. Individual or group projects

![](img/Henry2019-Table1.png)

## Assessment, grading policy and schedule

This class will have some assignments and projects (midterm and final). The final projects can be individual or group projects (depending on the number of students registered) and will be agreed upon with the instructor. There will be a midterm and a final oral presentation for each project. Moreover, a final report must be delivered. Instructions about what is expected for both midterm and final presentations as well as for the final report will be provided.

Grading breakdown:
- Participation (can include engagement in class, attendance, use of office hours, etc) (5%)
- Assignment 1 (10%): due Friday, September 13, by midnight (AOE)
- Assignment 2 (10%): due Friday, October 4, by midnight (AOE)
- Assignment 3 (10%): due Friday, November 08, by midnight (AOE)
- Assignment 4 (10%): due Wednesday, November 27, by midnight (AOE)

- **Midterm project** (%20): due Monday, October 28, by midnight (AOE)
- **Final project** (%35): due Monday, December 16 (exam hours 8-10 a.m.), but **Final project Proposal** is due six weeks prior, on Monday, November 04, by midnight (AOE). The Final project Proposal will need to be discussed with your teacher, before its submission by Monday, November 04. Please make sure to make plenty of use of Office Hours to discuss your Final project Proposal before submitting it.

Assignments will be distributed no later than a week prior to the due date.

The schedule is subject to change (the instructor will announce any changes).

**Late submission and absences policy**: if you submit your assignments late, there is an increasing penalty (10% off for up to 24 hours late, 20% off for 24-48 hours late). No assignments will be graded if submitted later than 48 hours late.

Any student who cannot attend class or submit assignments by their due date for serious issues (e.g., medical emergencies) or participation in university activities (e.g., official university travel for conferences or sports) that can be documented, should communicate those to your instructor as soon as possible before the deadline.

## GitHub

We'll use Git with GitHub Classroom for managing activities and feedback.

:::{tip}
If you don't have a GitHub account, follow these [instructions](https://sdsu-research-ci.github.io/github/students/creating-account) from the SDSU Research & Cyberinfrastructure [website](https://sdsu-research-ci.github.io/github) and [link it to your SDSUid](https://sdsu-research-ci.github.io/github/students/creating-account#linking-your-sdsuid).
- Use a personal email account rather than the SDSU one, so that you won't have problems accessing your GitHub account in the future.
- Choose your username wisely! Most likely you will use this again in professional settings in your career.
:::

## Course materials, programming languages and environment

I will provide all free course materials and suggested readings on the [class website](https://sdsu-comp526.github.io/fall24/). If you prefer to read a print-out version, please talk to me. I will primarily use Julia and [Jupyter notebooks](https://jupyter.org/) for slides and activities in class. This environment is convenient to work with, general purpose, and has extensive library support.  It is possible to write fast code in Julia, though performance implications can be mysterious. C, C++, and Fortran are popular languages for writing production numerical software, sometimes called from a higher level programming language like Python. MATLAB is also popular for numerical computing, though it is a proprietary environment and lacks general-purpose libraries.

Most HPC facilities use a Linux operating system and many open source software packages and libraries will have the best documentation and testing on Linux systems. You can use any environment for your local development environment, or use the SDSU's [JupyterHub](https://jupyterhub.sdsu.edu/) to experiment and develop without a local install. If you have never logged in before, check SDSU's Research & Cyberinfrastructure [resources for students](https://sdsu-research-ci.github.io/instructionalcluster/students).

## Target audience

Students in computational science, applied mathematics, or a quantitative science or engineering field.

Catalog Prerequisites:

* MATH 252: Calculus III
* MATH 254: Introduction to Linear Algebra

Good to know:

* Matrix theory
* Integration
* Partial differentiation

## Classroom Behavior

Both students and faculty are responsible for maintaining an appropriate learning environment in all instructional settings, whether in person, remote, or online. Those who fail to adhere to such behavioral standards may be subject to discipline. Professional courtesy and sensitivity are especially important with respect to individuals and topics dealing with race, color, national origin, sex, pregnancy, age, disability (visible or invisible), creed, religion, sexual orientation, gender identity, gender expression, veteran status, political affiliation, or political philosophy.

## Resources for students
Every student is encouraged to read the [SDSU Student Academic Success Handbook](https://docs.google.com/document/d/1rXNpNGs1K7nIxcS73o6R-fxZqPIWQwS9gHD7XpIqjhM/edit#heading=h.apbuhr7p11ak) (includes essential information for students). Please, watch this [video](https://drive.google.com/file/d/1eViqiJ3TDjuA6a-342aLZMpFTzGCXzUJ/view?usp=drivesdk
).

## Accommodation for Disabilities

If you think you may qualify for accommodations because of a disability, please contact [SDSU Student Ability Success Center](https://sds.sdsu.edu/) and make your faculty member aware in a timely manner so that your needs can be addressed. Please allow 10-14 business days for this process.


## Preferred Student Names and Pronouns

We recognize that students' legal information doesn't always align with how they identify. Class rosters are provided to the instructor with the student's legal name. If you feel that the name that appears on the class roster does not reflect your preferred name or pronoun, let your faculty member know.

## Academic Honesty

SDSU has strict codes of conduct and policies regarding [cheating and plagiarism](https://sacd.sdsu.edu/student-rights/academic-dishonesty/cheating-and-plagiarism). Become familiar with the policy and what constitutes plagiarism. Any cheating or plagiarism will result in failing this class and a disciplinary review by the University. These actions may lead to probation, suspension, or expulsion.

## Use of AI

In May 2024, the University Senate extended its definition of plagiarism to include the un-cited use of generative AI applications, specifically: "representing work produced by generative Artificial Intelligence as one’s own." Academic freedom ensures that instructors are empowered to determine whether students may use genAI in their classes and to what extent. To minimize confusion, we report here a statement regarding the use of AI in this class.

Students should not use generative AI applications in this course except as approved by the instructor and cited. Any use of generative AI outside of instructor-approved guidelines constitutes misuse. Misuse of generative AI is a violation of the course policy on academic honesty and will be reported to the Center for Student Rights and Responsibilities

## Sexual Misconduct, Discrimination, Harassment and/or Related Retaliation

SDSU is committed to fostering an inclusive and welcoming learning, working, and living environment. SDSU will not tolerate acts of sexual misconduct (harassment, exploitation, and assault), intimate partner violence (dating or domestic violence), stalking, or protected-class discrimination or harassment by or against members of our community. Individuals who believe they have been subject to misconduct or retaliatory actions for reporting a concern should contact the [SDSU Title IX Office](https://titleix.sdsu.edu/report-an-incident-landing).

Please know that faculty and responsible employees have a responsibility to inform the Title IX Office when made aware of incidents of sexual misconduct, dating and domestic violence, stalking, discrimination, harassment, and/or related retaliation, to ensure that individuals impacted receive information about their rights, support resources, and reporting options.

## Religious Holidays

According to the University Policy File, students should notify instructors of planned absences for religious observances by the end of the second week of classes. See the campus policy regarding religious observances for full details.

## Land Acknowledgment

For millennia, the Kumeyaay people have been a part of this land. This land has nourished, healed, protected and embraced them for many generations in a relationship of balance and harmony. As members of the San Diego State University community, we acknowledge this legacy. We promote this balance and harmony. We find inspiration from this land, the land of the Kumeyaay.

