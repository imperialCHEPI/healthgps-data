#!/usr/bin/env python3
"""
Format raw Downloads disease CSVs into healthgps-data layout.

D-files (allowlisted cancers only) -> diseases/{disease}/D{code}.csv
P-outputs (male/female merge)      -> diseases/{disease}/P{code}/
    prevalence_distribution.csv | death_weights.csv | survival_rate_parameters.csv
analysis/cost BoD*.csv             -> analysis/cost/
undb indicators/mortality/population -> unbd/{indicators,mortality,population}/

Never modifies source files. Skips destinations that already exist.

Note: output folder is named ``unbd`` to match the formatted_output layout
(healthgps-data upstream uses ``undb``).
"""

from __future__ import annotations

import argparse
import csv
import re
import shutil
import sys
from collections import defaultdict
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple


# ---------------------------------------------------------------------------
# D-file allowlist: snake_case source name -> healthgps disease folder
# ---------------------------------------------------------------------------
D_ALLOWLIST: Dict[str, str] = {
    "breast_cancer": "breastcancer",
    "lip_and_oral_cavity_cancer": "lipandoralcavitycancer",
    "cervical_cancer": "cervicalcancer",
    "colon_and_rectum_cancer": "colorectalcancer",
    "tracheal_bronchus_and_lung_cancer": "trachealbronchuslungcancer",
    "esophageal_cancer": "esophaguscancer",
    "gallbladder_and_biliary_tract_cancer": "gallbladdercancer",
    "kidney_cancer": "kidneycancer",
    "liver_cancer": "livercancer",
    "ovarian_cancer": "ovarycancer",
    "pancreatic_cancer": "pancreascancer",
    "stomach_cancer": "stomachcancer",
    "larynx_cancer": "larynxcancer",
    "other_pharynx_cancer": "otherpharynxcancer",
    "thyroid_cancer": "thyroidcancer",
}

# Only these disease folders appear under output diseases/ (D and P).
ALLOWED_DISEASE_FOLDERS: Set[str] = set(D_ALLOWLIST.values())

# Country display name (from *_outputs_2026 folder / filename) -> ISO numeric code
COUNTRY_NAME_TO_CODE: Dict[str, str] = {
    "belgium": "56",
    "estonia": "233",
    "finland": "246",
    "italy": "380",
    "malta": "470",
    "norway": "578",
    "poland": "616",
    "romania": "642",
    "slovenia": "705",
    "spain": "724",
    "uk": "826",
    "united kingdom": "826",
}

# Normalized P-source disease token -> healthgps folder (allowlisted cancers only).
# Keys are lowercase with non-alphanumeric stripped.
P_DISEASE_MAP: Dict[str, str] = {
    "breastcancer": "breastcancer",
    "cervicalcancer": "cervicalcancer",
    "cervixutericancer": "cervicalcancer",
    "colorectumcancer": "colorectalcancer",
    "gallbladdercancer": "gallbladdercancer",
    "kidneycancer": "kidneycancer",
    "larynxcancer": "larynxcancer",
    "liporalcavitycancer": "lipandoralcavitycancer",
    "liverandintrahepaticbileductscancer": "livercancer",
    "oesophaguscancer": "esophaguscancer",
    "esophaguscancer": "esophaguscancer",
    "otherpharynxcancer": "otherpharynxcancer",
    "ovarycancer": "ovarycancer",
    "pancreascancer": "pancreascancer",
    "stomachcancer": "stomachcancer",
    "thyroidcancer": "thyroidcancer",
    "tracheabronchusandlungcancer": "trachealbronchuslungcancer",
}

DFILE_RE = re.compile(
    r"^(?P<country>[^_]+)_(?P<code>\d+)__(?P<disease>.+)\.csv$",
    re.IGNORECASE,
)

PREV_RE = re.compile(
    r"^(?P<country>.+)_(?P<disease>.+)_(?P<sex>males|females)_PrevalenceDistribution\.csv$",
    re.IGNORECASE,
)
DEATH_RE = re.compile(
    r"^(?P<country>.+)_(?P<disease>.+)_(?P<sex>males|females)_DeathWeights\.csv$",
    re.IGNORECASE,
)
SURV_RE = re.compile(
    r"^(?P<country>.+)_(?P<disease>.+)_(?P<sex>Male|Female)_SurvivalRatesParameters\.csv$",
    re.IGNORECASE,
)

METRIC_OUT = {
    "prevalence": "prevalence_distribution.csv",
    "death": "death_weights.csv",
    "survival": "survival_rate_parameters.csv",
}

# Output folder name matching formatted_output (upstream repo uses "undb").
UNBD_OUTPUT_NAME = "unbd"
UNBD_SUBDIRS = ("indicators", "mortality", "population")
# Staging uses ``unbd``; live healthgps-data repo uses ``undb``.
LIVE_UNDB_NAME = "undb"

# Filename prefixes used under analysis/cost and unbd subfolders.
ANALYSIS_BOD_RE = re.compile(r"^BoD(?P<code>\d+)\.csv$", re.IGNORECASE)
UNBD_FILE_RE = re.compile(
    r"^(?P<prefix>Pi|M|P)(?P<code>\d+)\.csv$",
    re.IGNORECASE,
)


def normalize_disease_key(name: str) -> str:
    """Lowercase and strip non-alphanumeric characters."""
    return re.sub(r"[^a-z0-9]", "", name.lower())


def load_countries_csv(path: Path) -> Dict[str, str]:
    """Return lowercase country name -> code from countries.csv (optional check)."""
    mapping: Dict[str, str] = {}
    if not path.exists():
        return mapping
    with path.open(newline="", encoding="utf-8-sig") as fh:
        reader = csv.DictReader(fh)
        for row in reader:
            name = (row.get("Name") or "").strip().lower()
            code = str(row.get("Code") or "").strip()
            if name and code:
                mapping[name] = code
    return mapping


@dataclass
class Stats:
    created: List[str] = field(default_factory=list)
    skipped_existing: List[str] = field(default_factory=list)
    d_ignored: List[str] = field(default_factory=list)
    p_ignored: List[str] = field(default_factory=list)
    unmapped: List[str] = field(default_factory=list)
    single_sex: List[str] = field(default_factory=list)
    missing_metric: List[str] = field(default_factory=list)
    errors: List[str] = field(default_factory=list)

    def summary(self) -> str:
        lines = [
            "=== format_healthgps_disease_data summary ===",
            f"created:           {len(self.created)}",
            f"skipped_existing:  {len(self.skipped_existing)}",
            f"D_IGNORED:         {len(self.d_ignored)}",
            f"P_IGNORED:         {len(self.p_ignored)}",
            f"unmapped:          {len(self.unmapped)}",
            f"SINGLE_SEX:        {len(self.single_sex)}",
            f"missing_metric:    {len(self.missing_metric)}",
            f"errors:            {len(self.errors)}",
        ]
        if self.unmapped:
            lines.append("\nUnmapped (sample):")
            for item in self.unmapped[:20]:
                lines.append(f"  - {item}")
        if self.single_sex:
            lines.append("\nSINGLE_SEX (sample):")
            for item in self.single_sex[:20]:
                lines.append(f"  - {item}")
        if self.errors:
            lines.append("\nErrors:")
            for item in self.errors[:20]:
                lines.append(f"  - {item}")
        return "\n".join(lines)


def resolve_country_code(
    country_name: str,
    countries_csv: Dict[str, str],
) -> Optional[str]:
    key = country_name.strip().lower()
    if key in COUNTRY_NAME_TO_CODE:
        return COUNTRY_NAME_TO_CODE[key]
    if key in countries_csv:
        return countries_csv[key]
    # UK variants
    if key in {"united kingdom of great britain and northern ireland", "great britain"}:
        return "826"
    return None


def write_or_skip(
    dest: Path,
    *,
    dry_run: bool,
    stats: Stats,
    writer,
) -> bool:
    """
    If dest exists, skip. Otherwise create parent dirs and call writer(dest).
    writer is a callable that writes the file at dest.
    Returns True if created (or would create in dry-run).
    """
    rel = str(dest)
    if dest.exists():
        stats.skipped_existing.append(rel)
        return False
    if dry_run:
        stats.created.append(f"[dry-run] {rel}")
        return True
    dest.parent.mkdir(parents=True, exist_ok=True)
    writer(dest)
    stats.created.append(rel)
    return True


def process_dfiles(
    dfiles_dir: Path,
    output_root: Path,
    *,
    country_filter: Optional[Set[str]],
    dry_run: bool,
    stats: Stats,
) -> None:
    if not dfiles_dir.is_dir():
        stats.errors.append(f"Dfiles directory not found: {dfiles_dir}")
        return

    for path in sorted(dfiles_dir.glob("*.csv")):
        match = DFILE_RE.match(path.name)
        if not match:
            stats.errors.append(f"Dfile name not parsed: {path.name}")
            continue

        country = match.group("country").lower()
        code = match.group("code").lstrip("0") or "0"
        # Keep codes as in source (no leading zeros stripped incorrectly for
        # multi-digit). Source uses 56, 233, etc. — only strip leading zeros
        # that would turn "056" into "56"; numeric int then str is cleaner.
        code = str(int(match.group("code")))
        disease_snake = match.group("disease").lower()

        if country_filter and country not in country_filter:
            continue

        folder = D_ALLOWLIST.get(disease_snake)
        if folder is None:
            stats.d_ignored.append(path.name)
            continue

        dest = output_root / "diseases" / folder / f"D{code}.csv"

        def _copy(dst: Path, src: Path = path) -> None:
            shutil.copy2(src, dst)

        write_or_skip(dest, dry_run=dry_run, stats=stats, writer=_copy)


def read_time_value_csv(path: Path) -> Dict[str, str]:
    """
    Read a 2-column Time,Value CSV. Handles headerless files and survival
    junk headers such as '',x.
    """
    result: Dict[str, str] = {}
    with path.open(newline="", encoding="utf-8-sig") as fh:
        reader = csv.reader(fh)
        for row in reader:
            if not row or all(not (c or "").strip() for c in row):
                continue
            if len(row) < 2:
                continue
            time_raw = (row[0] or "").strip()
            value_raw = (row[1] or "").strip()
            # Skip header-like rows
            if time_raw.lower() in {"", "time", "x"} and not _looks_numeric(time_raw):
                continue
            if time_raw.lower() == "time":
                continue
            if not _looks_numeric(time_raw):
                # survival files sometimes start with blank first cell as header
                continue
            result[time_raw] = value_raw
    return result


def _looks_numeric(text: str) -> bool:
    try:
        float(text)
        return True
    except ValueError:
        return False


def merge_sex_series(
    male: Optional[Dict[str, str]],
    female: Optional[Dict[str, str]],
) -> Tuple[List[Tuple[str, str, str]], bool]:
    """
    Outer-join on Time.
    Female-only diseases: Male column is 0.
    Male-only diseases: Female column is 0.
    Returns (rows, is_single_sex).
    """
    male = male or {}
    female = female or {}
    female_only = bool(female) and not male
    male_only = bool(male) and not female
    times = sorted(
        set(male) | set(female),
        key=lambda t: (float(t) if _looks_numeric(t) else t),
    )
    rows: List[Tuple[str, str, str]] = []
    for t in times:
        if female_only:
            rows.append((t, "0", female.get(t, "")))
        elif male_only:
            rows.append((t, male.get(t, ""), "0"))
        else:
            rows.append((t, male.get(t, ""), female.get(t, "")))
    single = female_only or male_only
    return rows, single


def write_merged_csv(dest: Path, rows: List[Tuple[str, str, str]]) -> None:
    with dest.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.writer(fh, lineterminator="\n")
        writer.writerow(["Time", "Male", "Female"])
        for time, male, female in rows:
            writer.writerow([time, male, female])


def map_p_disease(raw_disease: str, stats: Stats, source_name: str) -> Optional[str]:
    key = normalize_disease_key(raw_disease)
    folder = P_DISEASE_MAP.get(key)
    if folder is None:
        stats.p_ignored.append(f"{source_name} -> key={key}")
        return None
    if folder not in ALLOWED_DISEASE_FOLDERS:
        stats.p_ignored.append(f"{source_name} -> folder={folder}")
        return None
    return folder


def sex_key(raw: str) -> str:
    low = raw.lower()
    if low.startswith("male"):
        return "male"
    if low.startswith("female"):
        return "female"
    return low


@dataclass
class PGroup:
    """Files for one (country_code, disease_folder, metric)."""
    files: Dict[str, Path] = field(default_factory=dict)  # male|female -> path


def collect_p_groups(
    source_diseases: Path,
    countries_csv: Dict[str, str],
    *,
    country_filter: Optional[Set[str]],
    stats: Stats,
) -> Dict[Tuple[str, str, str], PGroup]:
    """
    Key: (country_code, disease_folder, metric) where metric in
    prevalence|death|survival.
    """
    groups: Dict[Tuple[str, str, str], PGroup] = defaultdict(PGroup)

    for folder in sorted(source_diseases.iterdir()):
        if not folder.is_dir():
            continue
        if not folder.name.endswith("_outputs_2026"):
            continue

        country_from_folder = folder.name[: -len("_outputs_2026")]
        code = resolve_country_code(country_from_folder, countries_csv)
        if code is None:
            stats.errors.append(f"Unknown country folder: {folder.name}")
            continue
        if country_filter and country_from_folder.lower() not in country_filter:
            continue

        for path in sorted(folder.glob("*.csv")):
            metric = None
            match = None
            for kind, regex in (
                ("prevalence", PREV_RE),
                ("death", DEATH_RE),
                ("survival", SURV_RE),
            ):
                match = regex.match(path.name)
                if match:
                    metric = kind
                    break
            if match is None or metric is None:
                # Ignore unrelated CSVs quietly
                continue

            file_country = match.group("country")
            # Prefer folder country code; verify name roughly matches
            file_code = resolve_country_code(file_country, countries_csv)
            if file_code and file_code != code:
                stats.errors.append(
                    f"Country mismatch in {path.name}: folder={code} file={file_code}"
                )
                continue

            disease_folder = map_p_disease(match.group("disease"), stats, path.name)
            if disease_folder is None:
                continue

            sex = sex_key(match.group("sex"))
            key = (code, disease_folder, metric)
            groups[key].files[sex] = path

    return groups


def process_p_outputs(
    source_diseases: Path,
    output_root: Path,
    countries_csv: Dict[str, str],
    *,
    country_filter: Optional[Set[str]],
    dry_run: bool,
    stats: Stats,
) -> None:
    groups = collect_p_groups(
        source_diseases,
        countries_csv,
        country_filter=country_filter,
        stats=stats,
    )

    for (code, disease_folder, metric), group in sorted(groups.items()):
        out_name = METRIC_OUT[metric]
        dest = output_root / "diseases" / disease_folder / f"P{code}" / out_name

        male_path = group.files.get("male")
        female_path = group.files.get("female")

        if male_path is None and female_path is None:
            continue

        try:
            male_data = read_time_value_csv(male_path) if male_path else None
            female_data = read_time_value_csv(female_path) if female_path else None
        except OSError as exc:
            stats.errors.append(f"Failed reading {male_path or female_path}: {exc}")
            continue

        rows, single = merge_sex_series(male_data, female_data)
        if single:
            present = "male" if male_path else "female"
            stats.single_sex.append(
                f"{disease_folder}/P{code}/{out_name} ({present} only)"
            )

        if not rows:
            stats.missing_metric.append(
                f"empty merge: {disease_folder}/P{code}/{out_name}"
            )
            continue

        def _write(dst: Path, data: List[Tuple[str, str, str]] = rows) -> None:
            write_merged_csv(dst, data)

        write_or_skip(dest, dry_run=dry_run, stats=stats, writer=_write)


def parse_country_filter(raw: Optional[str]) -> Optional[Set[str]]:
    if not raw:
        return None
    return {part.strip().lower() for part in raw.split(",") if part.strip()}


def country_filter_to_codes(
    country_filter: Optional[Set[str]],
    countries_csv: Dict[str, str],
) -> Optional[Set[str]]:
    """Map country name filter to ISO numeric code strings."""
    if not country_filter:
        return None
    codes: Set[str] = set()
    for name in country_filter:
        code = resolve_country_code(name, countries_csv)
        if code is None:
            continue
        codes.add(code)
    return codes


def resolve_undb_source(source_undb: Path) -> Optional[Path]:
    """
    Accept either:
      source_undb/{indicators,mortality,population}
    or:
      source_undb/undb_update_*/{indicators,mortality,population}
    """
    if all((source_undb / sub).is_dir() for sub in UNBD_SUBDIRS):
        return source_undb
    updates = sorted(
        p for p in source_undb.glob("undb_update_*") if p.is_dir()
    )
    for candidate in reversed(updates):  # prefer newest name lexicographically
        if all((candidate / sub).is_dir() for sub in UNBD_SUBDIRS):
            return candidate
    return None


def process_analysis(
    source_analysis: Path,
    output_root: Path,
    *,
    code_filter: Optional[Set[str]],
    dry_run: bool,
    stats: Stats,
) -> None:
    """Copy BoD{code}.csv into analysis/cost/."""
    cost_src = source_analysis / "cost"
    if not cost_src.is_dir():
        # allow flat analysis/*.csv as well
        cost_src = source_analysis
    if not cost_src.is_dir():
        stats.errors.append(f"analysis source not found: {source_analysis}")
        return

    for path in sorted(cost_src.glob("BoD*.csv")):
        match = ANALYSIS_BOD_RE.match(path.name)
        if not match:
            stats.errors.append(f"analysis file not parsed: {path.name}")
            continue
        code = str(int(match.group("code")))
        if code_filter and code not in code_filter:
            continue
        dest = output_root / "analysis" / "cost" / path.name

        def _copy(dst: Path, src: Path = path) -> None:
            shutil.copy2(src, dst)

        write_or_skip(dest, dry_run=dry_run, stats=stats, writer=_copy)


def process_undb(
    source_undb: Path,
    output_root: Path,
    *,
    code_filter: Optional[Set[str]],
    dry_run: bool,
    stats: Stats,
) -> None:
    """Copy unbd indicators/mortality/population CSVs into output unbd/."""
    root = resolve_undb_source(source_undb)
    if root is None:
        stats.errors.append(
            f"undb source not found under {source_undb} "
            f"(need {list(UNBD_SUBDIRS)} or undb_update_*/...)"
        )
        return

    for sub in UNBD_SUBDIRS:
        sub_dir = root / sub
        for path in sorted(sub_dir.glob("*.csv")):
            match = UNBD_FILE_RE.match(path.name)
            if not match:
                stats.errors.append(f"unbd file not parsed: {sub}/{path.name}")
                continue
            code = str(int(match.group("code")))
            if code_filter and code not in code_filter:
                continue
            dest = output_root / UNBD_OUTPUT_NAME / sub / path.name

            def _copy(dst: Path, src: Path = path) -> None:
                shutil.copy2(src, dst)

            write_or_skip(dest, dry_run=dry_run, stats=stats, writer=_copy)


def map_staging_rel_to_live(rel: Path) -> Path:
    """Map staging relative path into live data path (unbd -> undb)."""
    parts = list(rel.parts)
    if parts and parts[0] == UNBD_OUTPUT_NAME:
        parts[0] = LIVE_UNDB_NAME
    return Path(*parts) if parts else Path(".")


def merge_staging_into_live(
    staging_root: Path,
    live_root: Path,
    *,
    dry_run: bool,
    stats: Stats,
) -> None:
    """
    Add files from formatted staging into the original live data folder.
    Never overwrites existing live files. Maps unbd/ -> undb/.
    """
    if not staging_root.is_dir():
        stats.errors.append(f"staging root not found: {staging_root}")
        return
    if not live_root.is_dir():
        stats.errors.append(f"live data root not found: {live_root}")
        return

    print(f"merge: {staging_root}  ->  {live_root}")
    print(f"       ({UNBD_OUTPUT_NAME}/ mapped to {LIVE_UNDB_NAME}/)")

    for path in sorted(staging_root.rglob("*")):
        if not path.is_file():
            continue
        rel = path.relative_to(staging_root)
        dest = live_root / map_staging_rel_to_live(rel)

        def _copy(dst: Path, src: Path = path) -> None:
            shutil.copy2(src, dst)

        write_or_skip(dest, dry_run=dry_run, stats=stats, writer=_copy)


def build_arg_parser() -> argparse.ArgumentParser:
    default_source = Path(r"c:\Users\mg423\Downloads\diseases")
    default_analysis = Path(r"c:\Users\mg423\Downloads\analysis")
    default_undb = Path(r"c:\Users\mg423\Downloads\undb")
    default_output = Path(r"C:\healthgps-data\formatted_output\data")
    default_countries = Path(r"C:\healthgps-data\data\countries.csv")

    parser = argparse.ArgumentParser(
        description="Format Downloads disease CSVs into healthgps-data layout."
    )
    parser.add_argument(
        "--source-diseases",
        type=Path,
        default=default_source,
        help=f"Root containing Dfiles/ and *_outputs_2026/ (default: {default_source})",
    )
    parser.add_argument(
        "--source-analysis",
        type=Path,
        default=default_analysis,
        help=f"Root containing cost/BoD*.csv (default: {default_analysis})",
    )
    parser.add_argument(
        "--source-undb",
        type=Path,
        default=default_undb,
        help=f"Root containing undb indicators/mortality/population (default: {default_undb})",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=default_output,
        help=f"Output data root (default: {default_output})",
    )
    parser.add_argument(
        "--countries-csv",
        type=Path,
        default=default_countries,
        help=f"countries.csv for name/code checks (default: {default_countries})",
    )
    parser.add_argument(
        "--countries",
        type=str,
        default=None,
        help="Comma-separated country filter, e.g. belgium,uk",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Report actions without writing files",
    )
    parser.add_argument(
        "--strict",
        action="store_true",
        help="Exit non-zero if any errors or unmapped P diseases",
    )
    parser.add_argument(
        "--skip-d",
        action="store_true",
        help="Skip D-file processing",
    )
    parser.add_argument(
        "--skip-p",
        action="store_true",
        help="Skip P-output processing",
    )
    parser.add_argument(
        "--skip-analysis",
        action="store_true",
        help="Skip analysis/cost copy",
    )
    parser.add_argument(
        "--skip-undb",
        action="store_true",
        help="Skip unbd indicators/mortality/population copy",
    )
    parser.add_argument(
        "--merge-into",
        type=Path,
        default=None,
        help=(
            "After formatting (or with --merge-only), add staging files into this "
            f"live data root (maps {UNBD_OUTPUT_NAME}/ -> {LIVE_UNDB_NAME}/; "
            "never overwrites)."
        ),
    )
    parser.add_argument(
        "--merge-only",
        action="store_true",
        help="Skip formatting; only merge --output staging into --merge-into",
    )
    return parser


def main(argv: Optional[Iterable[str]] = None) -> int:
    args = build_arg_parser().parse_args(list(argv) if argv is not None else None)
    stats = Stats()
    country_filter = parse_country_filter(args.countries)
    countries_csv = load_countries_csv(args.countries_csv)
    code_filter = country_filter_to_codes(country_filter, countries_csv)

    source: Path = args.source_diseases
    output: Path = args.output

    print(f"source diseases:  {source}")
    print(f"source analysis:  {args.source_analysis}")
    print(f"source undb:      {args.source_undb}")
    print(f"output:           {output}")
    print(f"dry_run: {args.dry_run}")
    if country_filter:
        print(f"countries filter: {sorted(country_filter)} -> codes {sorted(code_filter or [])}")

    if not args.merge_only:
        if not args.skip_d:
            process_dfiles(
                source / "Dfiles",
                output,
                country_filter=country_filter,
                dry_run=args.dry_run,
                stats=stats,
            )

        if not args.skip_p:
            process_p_outputs(
                source,
                output,
                countries_csv,
                country_filter=country_filter,
                dry_run=args.dry_run,
                stats=stats,
            )

        if not args.skip_analysis:
            process_analysis(
                args.source_analysis,
                output,
                code_filter=code_filter,
                dry_run=args.dry_run,
                stats=stats,
            )

        if not args.skip_undb:
            process_undb(
                args.source_undb,
                output,
                code_filter=code_filter,
                dry_run=args.dry_run,
                stats=stats,
            )

    if args.merge_into is not None:
        merge_staging_into_live(
            output,
            args.merge_into,
            dry_run=args.dry_run,
            stats=stats,
        )
    elif args.merge_only:
        stats.errors.append("--merge-only requires --merge-into")

    print(stats.summary())

    if args.strict and (stats.errors or stats.unmapped):
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
