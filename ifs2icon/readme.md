# ifs2icon_vert — Vertical Interpolation from IFS/CAMS to ICON Model Levels

## 1  How to Use the Interpolation

### 1.1  Required Input Files

The tool expects the following inputs, each containing exactly **one timestep**:

| Argument | Description |
|---|---|
| `fname_tq` | NetCDF file with temperature `t` and specific humidity `q` on IFS model levels. Used to compute geopotential heights via the hypsometric equation. |
| `fname_var` | NetCDF file with the variable(s) to be interpolated (e.g. ozone from CAMS). |
| `fname_surface` | NetCDF file with surface pressure `sp` and surface geopotential `z`. |
| `fname_ifs_model_level` | CSV of IFS/CAMS hybrid-level coefficients (columns: `n`, `a`, `b`). Export from ECMWF L60/L90/L137 model level tables; remove the unit row from the header. |
| `fname_icon_model_level` | ICON half-level heights file (HHL), typically `HHL_<grid>_l<nlevp1>.grb2`. |
| `target_level` | Integer number of ICON full levels (equal to the number of half levels minus one). |
| `fname_ifs_hgrid` | IFS horizontal grid description file for `iconremap`. |
| `fname_icon_hgrid` | ICON horizontal grid description file for `iconremap`. |
| `output_grid_name` | Label for the target ICON grid, used in output file names. |
| `var_nametable` | CSV mapping source (IFS/CAMS) variable names to target (ICON) variable names. Header: `source_name, target_name`. |

An example `var_nametable.csv`:

```
source_name, target_name
go3, o3
```

### 1.2  Invocation

```bash
python ifs2icon_vert.py \
    tq_field.nc \
    o3_field.nc \
    surface_parameter.nc \
    ifs_model_l60.csv \
    HHL_0024_R02B06_G_l91.grb2 \
    91 \
    ifs_grid.nc \
    icon_grid.nc \
    R02B06 \
    var_nametable.csv
```

### 1.3  External Dependencies

The script requires:

- **DWD ICON Tools** (`iconremap`) for horizontal interpolation.
- **CDO** (Climate Data Operators, MPI-M) for vertical interpolation (`intlevelx3d`) and grid format conversions.
- **Python**: `xarray`, `numpy`, `pandas`.

### 1.4  Output

For each variable listed in `var_nametable`, the final output is:

```
icon_level_remap_<output_grid_name>_<varname>.nc
```

This file contains the variable on the ICON target grid and vertical levels.

---

## 2  How the Interpolation Works

The overall procedure has three stages: horizontal remapping of all fields onto the ICON grid, computation of IFS geopotential heights on model levels, and vertical interpolation from IFS heights to ICON heights using CDO.

### 2.1  Stage 1 — Horizontal Interpolation

Before any vertical work is done, all input fields are remapped from the IFS/CAMS horizontal grid to the ICON triangular grid using DWD's `iconremap` tool (`intp_method = 3`, i.e. RBF interpolation). Three separate `iconremap` calls handle:

1. The meteorological fields `t` and `q`.
2. The surface fields `sp` and `z`.
3. The tracer or variable field(s) specified in `var_nametable`.

Each call writes a Fortran namelist (`NAMELIST_ifs2icon_horizontal`) and a variable table (`NAMELIST_ICON_VARIABLE_TABLE`) that `iconremap` reads. After this stage every field lives on the same ICON horizontal grid.

### 2.2  Stage 2 — Geopotential on IFS Model Levels

This is the physical core of the script. The IFS uses hybrid sigma-pressure (η) coordinates, so the geometric height of each model level is not stored explicitly — it must be reconstructed from the hydrostatic equation using temperature, humidity, surface pressure, and surface geopotential. The computation follows the standard ECMWF procedure (IFS Documentation, Part III, Sec. 2.2).

#### 2.2.1  Half-Level Pressure

IFS model levels are defined by pairs of coefficients (aₖ, bₖ). The pressure at each half level k is:

```
p_half(k) = a(k) + b(k) · pₛ
```

where pₛ is the surface pressure. At the model top (k = 0), a ≈ 0 and b = 0, so the pressure tends to zero. At the surface (k = N), a = 0 and b = 1, recovering pₛ. The coefficients transition smoothly between a pure pressure coordinate aloft and a terrain-following σ-coordinate near the surface.

#### 2.2.2  Virtual Temperature

To account for the density effect of water vapour, temperature is replaced by the virtual temperature:

```
Tᵥ = T · (1 + 0.609133 · q)
```

where 0.609133 = Rᵥ/R_d − 1 with R_d = 287.06 J kg⁻¹ K⁻¹.

#### 2.2.3  Upward Integration of the Hydrostatic Equation

The geopotential on full levels (where model variables are defined) is obtained by integrating upward from the surface geopotential Φₛ. The integration proceeds in three stages:

**Lowest full level (k = N).** The boundary condition is set by the surface geopotential. Define:

```
Δln p(k) = ln[ p_half(k) / p_half(k−1) ]

α(k) = 1 − [ p_half(k−1) / (p_half(k) − p_half(k−1)) ] · Δln p(k)
```

Then:

```
Φ_full(N) = Φₛ + α(N) · R_d · Tᵥ(N)
Φ_half(N) = Φₛ + Δln p(N) · R_d · Tᵥ(N)
```

The factor α represents the fractional position of the full level within the layer bounded by two half levels; it arises from the assumption that temperature is constant within each layer (the standard IFS discretisation of the hydrostatic integral).

**Intermediate levels (k = N−1 down to k = 2).** Each step uses the half-level geopotential from the layer below:

```
Φ_full(k) = Φ_half(k+1) + α(k) · R_d · Tᵥ(k)
Φ_half(k) = Φ_half(k+1) + Δln p(k) · R_d · Tᵥ(k)
```

This is the discrete analogue of integrating dΦ = −(1/ρ) dp upward through each layer, with the layer-mean virtual temperature providing the thickness.

**Top level (k = 1).** The upper boundary uses a reference pressure of 0.1 Pa and sets α = ln 2, consistent with the ECMWF convention that the top full level sits at the geometric mean pressure of the bounding half levels:

```
Δln p(1) = ln[ p_half(1) / 0.1 ]
α(1) = ln 2

Φ_full(1) = Φ_half(2) + α(1) · R_d · Tᵥ(1)
```

The result is a 3-D field of geopotential Φ(x, y, k) on IFS full levels, which is written to a NetCDF file (`geopotential_<fname_tq>`).

### 2.3  Stage 3 — Vertical Interpolation via CDO

With both the source (IFS) and target (ICON) height structures known, CDO's `intlevelx3d` operator performs the actual vertical remapping.

#### 2.3.1  Constructing Height Fields

The ICON grid stores half-level heights (HHL). Full-level heights (HFL) are computed as the arithmetic mean of adjacent half levels:

```
cdo -divc,2.0 -add -sellevidx,1/<N−1> HHL -sellevidx,2/<N> HHL  HFL_ICON.nc
```

The IFS geopotential is converted to geometric height by dividing by the gravitational acceleration:

```
cdo divc,9.80665 -selname,z geopotential.nc  HFL_CAMS.nc
```

#### 2.3.2  3-D Level Interpolation

CDO's `intlevelx3d` interpolates a source field from one set of 3-D height surfaces to another:

```
cdo intlevelx3d,HFL_CAMS.nc -selname,<var> remapped_variable.nc HFL_ICON.nc  output.nc
```

The operator determines, for each horizontal grid point, which source levels bracket each target level and interpolates accordingly. Because both height fields are fully three-dimensional, the interpolation correctly handles the terrain-following nature of both grids — the mapping is different at every grid column.

### 2.4  Summary of the Processing Chain

```
IFS/CAMS input (IFS grid, η levels)
        │
        ├─ iconremap ──► t, q, sp, z, var  on ICON horizontal grid
        │
        ├─ Hydrostatic integration ──► Φ(x,y,k) on IFS full levels
        │
        ├─ Φ / g ──► HFL_CAMS  (IFS geometric heights)
        │
        ├─ (HHL_k + HHL_{k+1}) / 2 ──► HFL_ICON  (ICON geometric heights)
        │
        └─ cdo intlevelx3d ──► var on ICON grid and ICON vertical levels
```
