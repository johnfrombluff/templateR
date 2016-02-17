// FIXME: read the templates and lists from files, and output to a file.

package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"regexp"
	"strconv"
	"strings"
)

func main() {
	fmt.Println("# \tWelcome to Template Processor\t")

	lang := "R"
	filenames := "univariate-tests-0-to-2km"
	R_output_file := filenames+".txt"
	R_input_file :=  filenames+".R"

	tmp_t_spss := "GENLINMIXED\n  /DATA_STRUCTURE SUBJECTS=school\n  /FIELDS TARGET=ATvsMT\n  /TARGET_OPTIONS DISTRIBUTION=BINOMIAL LINK=LOGIT\n  /FIXED  EFFECTS=Dist2School_km Age_at_Survey gender"

	tmp_b_spss := "USE_INTERCEPT=TRUE\n  /EMMEANS_OPTIONS SCALE=TRANSFORMED PADJUST=LSD\n."

	tmp_t := "require(foreign)\ndir  <- '/home/john/Dropbox/Research/Collaboration/BEATS/John/Analysis/Correlates of ATS/'\nfile <- 'BEATS_SS_ForWalk2School_160216_COMPLETEwithGIS.sav'\ndat <- read.spss( paste(dir, file, sep='/'), to.data.frame=TRUE )\nrm(dir, file )\n"
	tmp_p_R := tmp_t + "source('Scripts/data-setup-W2S.R')\nrequire(rms)\noptions( contrasts=c('contr.treatment', 'contr.treatment'))\ndat.m <- dat[ complete.cases(dat$Include) & Distance2School_GIS <= 2000, ]\nm.ddist <- datadist(dat.m)\noptions(datadist='m.ddist')\nsink(file='" + R_output_file + "', append=FALSE)\n"

	tmp_t_R_con := "print(robcov(lrm(ATS_f ~ Dist2School_GIS + gender + Age_at_Survey + as.numeric("
	tmp_b_R_con := "), x=T, y=T, data=dat.m ), dat.m$school))"
	tmp_t_R_cat := "print(robcov(lrm(ATS_f ~ Dist2School_GIS + gender + Age_at_Survey + "
	tmp_b_R_cat := ", x=T, y=T, data=dat.m ), dat.m$school))"

	IVs_PF := "HMcars, WSpsh, WSpunsafe, WSpwalk, Dparents, Dparentsenjoy"
	IVs_YF := ", health, WSAint, WSAnice, WSAstim, WSAhealthy, WSAgood, WSAuseful, WSAsafe, WCSone, WSexercise, WSchat, WSfsh, WSbfri, WSbcool, WSf5ws, WSeschool, WSbwant, WStired, WSno, WSbstuff, WSbsweat, WSbplan, WSbsched, WSblocker, Dlikedriven"
	IVs_PE := ", F1_esthetics, F2_trafficped, F3_landuseaccess, F4_resdensity, F5_trafficsafety, F6_personalsafety, F7_connectivity, F8_footpaths, F9_hills, schoolsize, WSbdist, WStime, WSbsafe, WSbfootp, WCSlights, WCStraffic, WCScross, WCSdogs, WCShills, WCSrbor, WSbweather"
	IVs_cont := IVs_PF + IVs_YF + IVs_PE

	/*IVs_others := TsLike, tsdecision, WScontrol, WSintend, WSwant, WSconf, WS2wks, Icardrive, Icarown, scsib, hpeople, hadults, hsiblings, raincoat, schwho, schiclose, WCSdogs, TgCarMy, TgCarOth, TgWalk, TgBike, TgSkateb, TgScooter, TgBusPub, TgTaxi, TgPref, TgPrefR, Dptuse, Datuse, Dbadenv, Dcarbadenv
	 */
	IVs_cat := "NZDepCat, eth3, BMI_2cat_Overweight"

	var lines []string

	switch lang {
	case "SPSS":
		for _, v := range strings.Split(IVs_cont, ", ") {
			fmt.Print(tmp_t_spss)
			fmt.Print(" " + v + " ")
			fmt.Print(tmp_b_spss + "\n\n")
		}
	case "R":
		// Create the R script
		lines = append(lines, fmt.Sprintf(tmp_p_R))
		for _, v := range strings.Split(IVs_cont, ", ") {
			lines = append(lines, fmt.Sprintf(tmp_t_R_con+v+tmp_b_R_con+"\n"))
		}
		for _, v := range strings.Split(IVs_cat, ", ") {
			lines = append(lines, fmt.Sprintf(tmp_t_R_cat+v+tmp_b_R_cat+"\n"))
		}

		lines = append(lines, fmt.Sprintf("sink()\n"))

		// Write it to a file
		fo, err := os.Create(R_input_file)
		if err != nil {
			log.Fatal(err)
		}
		defer fo.Close()
		for _, line := range lines {
			fmt.Fprintln(fo, line)
		}

		// Run R on it
		runit := true
		if runit {
			fmt.Println("Running R")
			cmd := exec.Command("R", "--slave", "CMD", "BATCH", R_input_file)
			output, err := cmd.CombinedOutput()
			if err == nil {
				fmt.Println("R completed without errors")
			} else {
				fmt.Println("R encountered an error:" + string(output))
				return
			}
		}

		// Now write the multivariate model to the R file.
		vars := readResults(IVs_cont, R_output_file)
		mvm := "print(robcov(lrm(ATS_f ~ Dist2School + gender + Age_at_Survey + " + strings.Join(vars, " + ") + ", x=T, y=T, data=dat.m ), cluster=dat.m$school))"

		ats_vars := "ats_vars <- c( 'ATS_f', 'Dist2School', 'gender', 'Age_at_Survey', '" + strings.Join(vars, "', '") + "')"
		f, err := os.OpenFile(R_input_file, os.O_APPEND|os.O_WRONLY, 0666)
		if err == nil {
			f.WriteString(mvm + "\n\n" + ats_vars + "\n")
		} else {
			fmt.Println("Error appending to " + R_input_file + ": ")
			fmt.Println("error message is: ", err)
		}
	}
}

func readResults(vars string, filename string) []string {
	fmt.Println("Processing " + filename)

	// Open the file and slurp it in
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		log.Fatal(err)
		fmt.Println("can't find file: " + filename)
	}
	lines := strings.Split(string(content), "\n")

	vars_p := strings.Split(vars, ", ")
	fmt.Println("Scanning results")
	var sigvars01 []string
	var sigvars05 []string
	for _, line := range lines {
		// Search for IVs in line
		vars = strings.Join(vars_p, "|")
		re_IV := regexp.MustCompile(`^(` + vars + `)\s+`)
		re_lt := regexp.MustCompile(`^<0\.0001`)

		if re_IV.FindStringIndex(string(line)) != nil {
			pa := strings.Fields(line)
			pv := pa[4]
			iv := pa[0]

			// replace <0.0001 and convert to numeric
			pv = re_lt.ReplaceAllString(pv, "0.0000")
			pv_i, _ := strconv.ParseFloat(pv, 64)
			//FIXME: determine field width by maximum length of IV name
			fw := "17"
			if pv_i < 0.01 {
				fmt.Printf("%"+fw+"s: %s***\n", iv, pv)
				sigvars01 = append(sigvars01, iv)
			} else if pv_i < 0.05 {
				fmt.Printf("%"+fw+"s: %s**\n", iv, pv)
				sigvars05 = append(sigvars05, iv)
			}
		}
	}

	return (append(sigvars01, sigvars05...))
}
