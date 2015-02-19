sourceGenerators in Compile <+= (sourceManaged in Compile).map(Boilerplate.gen)
