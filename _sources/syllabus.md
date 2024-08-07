# Syllabus

## Logistics

Comp-526
*Class time*: **M-W-F: 9-9:50 am**
*Location*: **LH 247**


### Instructor: [Valeria Barra](https://valeriabarra.org)
Email: vbarra @ sdsu . edu
Office: GMCS 593


**Office Hours**: by appointment.

:::{tip}
Office hours are an important time for asking questions, solving problems, discussion of broader academic and career strategy, and to provide feedback so I can make the class serve your needs and those of people with similar experiences and interests.
:::

## Overview

Scientific computing and mathematical modeling (both deterministic and stochastic) are fundamental tools for the solution of problems arising in the study of complex systems, whether originating from the physical, chemical, or biological sciences, or of an economic and social nature.

_Scientific computing_ is a broad term that describe the use of computers for scientific, medical, and engineering applications. This definition is often application-focused or domain-driven.

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
In reality this area encompasses all three: based on an application,
we formulate some mathematical problem, and find an algorithm for
it to solve it using computers.


## Organization

We will start by giving an introduction of version control and reproducibility, key apects in modern computational sciences.
Then we will introduce the Linux filesystem and some basic shell commands.

We'll procede with evaluation of functions and introduce the concepts of conditioning and stability, which we'll apply to every topic we encounter.

Then we'll explore rootfinding, our first infinite algorithm, in which we'll learn about convergence classes and the fundamental challenge of writing a function that is correct for all well-typed inputs.

We'll move to linear algebra and explore algorithms for computing QR factorization in depth -- these will offer geometric intuition and provide examples of stability and backward stability as well as performance in practice.

Next up will be interpolation and least squares regression using linear models, algorithms that transform data and an important subjective choice into functions, with pathologies that we'll identify and disentangle.

Calculus will show up next where we'll discuss numerical, analytic, and "automatic" differentiation, introduce Jacobian and Hessian matrices, and extend rootfinding to systems of equations.

This will enable us to solve nonlinear regression problems and compare the properties of linear vs nonlinear models.

We'll move on to integration, with a beautiful linear algebra connection, and finally numerical solution of differential equations.

Although we'll continue with new content, the second half of the semester will incorporate more project-based learning.
You'll then form small teams of like interest and work on an original study (numerical experiments and interpretation, comparisons, etc.) or on contribution to be shared with the community.
Studies and contributions can take many forms.

## Outcomes

Upon completing this course, students will be able to

* formulate problems in science and engineering in terms of computational methods
* evaluate accuracy and performance of algorithms
* diagnose ill conditioned problem formulations and unstable algorithms
* develop effective numerical software, taking into account stability, accuracy, and cost
* communicate about the above using figures, numerical experiments, writing, and presentation
* search for and understand relevant literature and documentation

### Expectations

1. Enter with a growth mindset, practice adaptive coping, and nurture your intrinsic motivation
2. Attend class (in-person) and participate in discussions
3. Make an honest attempt on activities, projects, etc.
4. Interact with the class notebooks and read reference material
5. Individual or group projects

![](img/Henry2019-Table1.png)

## Grading
This class will have some assignments and projects (midterm and final). The final projects can be individual or group projects (depending on the number of students registered) and will be agreed upon with the instructor. There will be a midterm and a final oral presentation
for each project. Moreover, a final report must be delivered. Instructions about what is expected for both midterm and final presentations as well as for the final report will be provided.

Grading breakdown:
- Assignment 1 (10%)
- Assignment 2 (10%)
- Assignment 3 (10%)

- Midterm project (%30)
- Final project (%40)


## GitHub

We'll use Git with GitHub Classroom for managing activities and feedback.

## Programming languages and environment

I will primarily use Julia and [Jupyter notebooks](https://jupyter.org/) for slides and activities in class. This environment is convenient to work with, general purpose, and has extensive library support.  It is possible to write fast code in Julia, though performance implications can be mysterious. C, C++, and Fortran are popular languages for writing production numerical software, sometimes called from a higher level programming language like Python.  MATLAB is also popular for numerical computing, though it is a proprietary environment and lacks general-purpose libraries.

Most HPC facilities use a Linux operating system and many open source software packages and libraries will have the best documentation and testing on Linux systems. You can use any environment for your local development environment, or use the SDSU's [JupyterHub](https://jupyterhub.sdsu.edu/) to experiment and develop without a local install. If you have never logged-in before, check SDSU's Research & Cyberinfrastructure [resources for students](https://sdsu-research-ci.github.io/instructionalcluster/students).

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

Both students and faculty are responsible for maintaining an appropriate learning environment in all instructional settings, whether in person, remote or online. Those who fail to adhere to such behavioral standards may be subject to discipline. Professional courtesy and sensitivity are especially important with respect to individuals and topics dealing with race, color, national origin, sex, pregnancy, age, disability, creed, religion, sexual orientation, gender identity, gender expression, veteran status, political affiliation or political philosophy.

## Accommodation for Disabilities

If you think you may qualify for accommodations because of a disability, please contact [SDSU Student Ability Success Center](https://sds.sdsu.edu/) and make your faculty member aware in a timely manner so that your needs can be addressed. Please allow 10-14 business days for this process.


## Preferred Student Names and Pronouns

We recognize that students' legal information doesn't always align with how they identify. Class rosters are provided to the instructor with the student's legal name. If you feel that the name that appears on the class roster does not reflect your preferred name or pronoun, let your faculty member know.

## Academic Honesty

SDSU has strict codes of conduct and policies regarding [cheating and plagiarism](https://sacd.sdsu.edu/student-rights/academic-dishonesty/cheating-and-plagiarism). Become familiar with the policy and what constitutes plagiarism. Any cheating or plagiarism will result in failing this class and a disciplinary review by the University. These actions may lead to probation, suspension, or expulsion.

## Use of AI
In May 2024, the University Senate extended its definition of plagiarism to include the un-cited use of generative AI applications, specifically: "representing work produced by generative Artificial Intelligence as one’s own." Academic freedom ensures that instructors are empowered to determine whether students may use genAI in their classes and to what extent. To minimize confusion, we report here a statement regarding the use of AI in this class.

Students should not use generative AI applications in this course except as approved by the instructor. Any use of generative AI outside of instructor-approved guidelines constitutes misuse. Misuse of generative AI is a violation of the course policy on academic honesty and will be reported to the Center for Student Rights and Responsibilities

## Sexual Misconduct, Discrimination, Harassment and/or Related Retaliation

SDSU is committed to fostering an inclusive and welcoming learning, working, and living environment. SDSU will not tolerate acts of sexual misconduct (harassment, exploitation, and assault), intimate partner violence (dating or domestic violence), stalking, or protected-class discrimination or harassment by or against members of our community. Individuals who believe they have been subject to misconduct or retaliatory actions for reporting a concern should contact the [SDSU Title IX Office](https://titleix.sdsu.edu/report-an-incident-landing).

Please know that faculty and responsible employees have a responsibility to inform the Title IX Office when made aware of incidents of sexual misconduct, dating and domestic violence, stalking, discrimination, harassment and/or related retaliation, to ensure that individuals impacted receive information about their rights, support resources, and reporting options.

## Religious Holidays

According to the University Policy File, students should notify instructors of planned absences for religious observances by the end of the second week of classes. See the campus policy regarding religious observances for full details.

## Land Acknowledgment
For millennia, the Kumeyaay people have been a part of this land. This land has nourished, healed, protected and embraced them for many generations in a relationship of balance and harmony. As members of the San Diego State University community, we acknowledge this legacy. We promote this balance and harmony. We find inspiration from this land, the land of the Kumeyaay.

