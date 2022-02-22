
ABELLA = abella
PROOF_OFFICIAL = proof_2.0.8_dev
PROOF_MODDED = proof_modded

SRC_OFFICIAL = $(wildcard $(PROOF_OFFICIAL)/*.thm)
SRC_MODDED = $(wildcard $(PROOF_MODDED)/*.thm)


all: proof_offcial


$(PROOF_MODDED)/%.thc: $(SRC_MODDED)
	$(ABELLA) $(@:.thc=.thm) -c $@ -o $(@:.thc=.out)

$(PROOF_OFFICIAL)/%.thc: $(SRC_OFFICIAL)
	$(ABELLA) $(@:.thc=.thm) -c $@ -o $(@:.thc=.out)

proof_offcial: $(PROOF_OFFICIAL)/decidability.thc $(PROOF_OFFICIAL)/stableSub.thc $(PROOF_OFFICIAL)/safety.thc

proof_modded: $(PROOF_MODDED)/decidability.thc $(PROOF_MODDED)/stableSub.thc $(PROOF_MODDED)/safety.thc

clean:
	rm $(PROOF_OFFICIAL)/*.thc $(PROOF_OFFICIAL)/*.out $(PROOF_MODDED)/*.thc $(PROOF_MODDED)/*.out

.phony: proof_offcial proof_modded clean
