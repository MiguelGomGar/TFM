"""Figure generation, one script per computing pipeline.

Every module here reads the tables its sibling in
``src/pipelines/calculations/`` wrote to ``results/`` and turns them into
figures. Nothing is recomputed and nothing is refitted, so the whole set of
figures can be regenerated in seconds with ``python run_plots.py``.
"""
