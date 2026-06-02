"""
Independent re-derivation of every hard-coded number on the cost-benefit slides.
Run AFTER you regenerate CSVs on your Mac. Compares slide values to recomputed ones.
"""
import math

PASS=[]; FAIL=[]; NOTE=[]
def chk(label, slide_val, computed, tol=0.06):
    if computed==0: ok = abs(slide_val)<1e-9
    else: ok = abs(slide_val-computed)/abs(computed) <= tol
    (PASS if ok else FAIL).append((label, slide_val, round(computed,3)))

# ---- INPUTS (each must be independently sourced; see provenance column) ----
EY_median   = 0.07     # Evans & Yuan 2022, access outcomes median SD  [SOURCE: WBER paper]
phi         = 0.85     # transport factor (JUDGMENT — flagged, not a data point)
mincer      = 0.075    # Bolivia EH 2024 female-only, from mincer_full.csv  [VERIFY vs CSV]
wage        = 3500     # median female annual labour income, Bolivia       [VERIFY vs EH2024]
lfp         = 0.65     # female labour force participation                  [VERIFY vs EH2024]
disc        = 0.03; T = 35
cost_full   = round((48+20+10+15+8)*4*1.15)   # 465
cost_ped    = round((10+15+8)*4*1.15)         # 152

# ---- DERIVED ----
sd_c = EY_median*phi                       # 0.0595 -> slide says 0.06
att_pp_c = 2.4                             # slide; check pp mapping below
# attendance pp from SD: pp = SD * sd_of_binary(p=0.80)=0.40 *100
att_pp_implied = sd_c*math.sqrt(0.80*0.20)*100
years = (att_pp_c/100)*4                    # 0.096 -> slide 0.10
annuity = (1-(1+disc)**-T)/disc
pv_school = wage*lfp*mincer*years*annuity
neet_pp=1.4; a3=(1-(1+disc)**-3)/disc
pv_neet = (neet_pp/100)*(wage*0.50*a3)
benefit = pv_school+pv_neet
bcr_ped = benefit/cost_ped
bcr_full= benefit/cost_full

# ---- CHECKS against slide-printed values ----
chk("phi-adjusted SD (slide 0.06)", 0.06, sd_c)
chk("attendance pp central (slide 2.4)", 2.4, att_pp_implied, tol=0.15)
chk("school-years (slide 0.10)", 0.10, years, tol=0.10)
chk("PV schooling (slide $352)", 352, pv_school, tol=0.08)
chk("PV NEET (slide $69)", 69, pv_neet, tol=0.10)
chk("total benefit (slide $421)", 421, benefit, tol=0.08)
chk("BCR pedagogy (slide 2.8)", 2.8, bcr_ped, tol=0.08)
chk("BCR full (slide 0.9)", 0.9, bcr_full, tol=0.10)
chk("cost full (slide $465)", 465, cost_full)
chk("cost pedagogy (slide $152)", 152, cost_ped)
chk("cost per pp full (slide $194)", 194, cost_full/2.4, tol=0.05)
chk("cost per pp ped (slide $63)", 63, cost_ped/2.4, tol=0.05)
chk("cost per NEET pp full (slide $332)", 332, cost_full/1.4, tol=0.05)
chk("cost per NEET pp ped (slide $109)", 109, cost_ped/1.4, tol=0.05)

NOTE.append("phi=0.85 is a JUDGMENT call, not a data point — document in A8.")
NOTE.append("wage=$3500, lfp=0.65 must be confirmed against EH2024 weighted estimates.")
NOTE.append("NEET pass-through 60% and 3yr/50%-wage idleness are MODELING assumptions.")
NOTE.append("mincer=0.075 must equal output/projections/mincer_full.csv aestudio coef.")

print("="*64); print("SLIDE NUMBER VERIFICATION"); print("="*64)
print(f"\nattendance pp implied by 0.06 SD at p=0.80: {att_pp_implied:.2f} (slide uses 2.4)")
print(f"\n✓ PASS ({len(PASS)}):")
for l,s,c in PASS: print(f"   {l}: slide={s} computed={c}")
if FAIL:
    print(f"\n✗ FAIL ({len(FAIL)}):")
    for l,s,c in FAIL: print(f"   {l}: slide={s} computed={c}")
else:
    print("\n✗ FAIL (0): none")
print(f"\n⚠ ASSUMPTIONS TO VERIFY MANUALLY:")
for n in NOTE: print(f"   - {n}")
