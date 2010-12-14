cat $1 | tr 'A-Z' 'a-z' | awk '\
BEGIN {actions="#"; entities="#"; parameters="#"} \
/\/attributeset/ {at=0} \
/<attributeset>/ {at=1} \
/<entities>/ {ent=1} \
/\/entities/ {ent=0} \
/<skillset>/ {sks=1} \
/\/skillset/ {sks=0} \
/<skill>/ {sk=1; thisparameters="#"} \
/\/skill/ {sk=0} \
/\/skill>/ && thisparameters != "#" {print "mandatory_parameters("skill",["thisparameters"])."} \
/\/skill>/ && thisparameters == "#" {print "mandatory_parameters("name",[])."} \
/<attributes>/ {att=1; attributelist="#"} \
/\/attributes/ {att=0} \
/<parameter>/ {para=1} \
/\/parameter/ {para=0} \
/<constant>/ {cons=1} \
/\/constant/ {cons=0} \
/name/ {split($0,v,"</*name>"); name=v[2]} \
/entry/ {split($0,v,"</*entry>"); entry=v[2]; gsub(" ", "_", entry); print "synonym("entry","name")."} \
at==1 && /description/ {split($0,v,"</*description>"); desc=v[2]; print "attribute("name",\""desc"\")."} \
ent==1 && /<id>/ {split($0,v,"</*id>"); id=v[2]; print "entity("name",\""id"\")."; entities=entities","name} \
sks==1 && /command/ {split($0,v,"</*command>"); command=v[2]; print "skill("name",\""command"\")."; actions=actions","name} \
/<arguments>/ {skill=name} \
ent==1 && att==1 && /<att>/ {split($0,v,"</*att>"); has=v[2]; attributelist=attributelist","has} \
sks==1 && att==1 && /<att>/ {split($0,v,"</*att>"); has=v[2]; attributelist=attributelist","has} \
/preposition/ {split($0,v,"</*preposition>"); prep=v[2]; print "preposition("skill","name","prep")."} \
para==1 && /significance/ {split($0,v,"</*significance>"); sig=v[2]; print "parameter("skill","name","sig")."; parameters=parameters","name} \
cons==1 && /significance/ {split($0,v,"</*significance>"); sig=v[2]} \
sk == 1 && /mandatory/ { thisparameters=thisparameters","name} \
cons==1 && /string/ {split($0,v,"</*string>"); string=v[2]; print "constant("skill","name","sig","string")."} \
/\/attributes/ && para==1 {print "parameter_attributes("skill","name",["attributelist"])."} \
/\/attributes/ && ent==1 {print "entity_attributes("name",["attributelist"])."} \
END {print ":- setval(all_actions, ["actions"])."; \ 
print ":- setval(all_entities, ["entities"])."; \
print ":- setval(all_parameters, ["parameters"])."}'| sed 's/reject),/reject,/g' | sed 's/#,//g' | sort -V

# print "parameter_attribute("skill","name","has")."
# print "entity_attribute("name","has")."
