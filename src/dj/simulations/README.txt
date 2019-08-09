One major pipeline for running simulations is generating the necessary ionic model files:

The pipeline is as follows: <data> [transformer]
<.model file>
[1 eat]
<.model text>
[2 dotmodel-ods]
<grouped text>
[3 pairs->exp-map]
<grouped expressions/Intermediate Representation>
[4]
<compiled IR>
[5]
<txt>
[6]
<file>

Also, there are convenience transformers that span more than one step:
[1-2 add-ods-data]
[1-3 config->assignments]
