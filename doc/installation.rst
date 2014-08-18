Installation
************


Install the ReverseDiffSource package at the Julia command line by running::

	Pkg.Install("ReverseDiffSource")

This only needs to be done once. 

``ReverseDiffSource`` has currently no dependency with other packages. However there is a graphical representation function ``plot()`` that produces a text string in the GraphViz syntax. To produce the plot, you will have to install GraphViz with ``Pkg.add``.

Now for each Julia session where ReverseDiffSource is needed, load it with the usual::

	using ReverseDiffSource

You are now ready to go !