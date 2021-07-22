# Whats in a name?

To minimize confusion, future malaise and overall cognitive burden we need to standardize object/entity naming practices for all things related to big query.

## Tables

[table type] _ [Table domain]

### Table types

Table types are the first part of the table name.  We currently have 3 types of tables

  * Fact tables (f_): These are storage optimized tables containing observational data.  
  * Dimension tables (d_): These are "shared" tables and provide additional data richness or definition to other tables (when joined).
  * Views (v_): Views are "virtual" query generated tables. They are the datasets intended for use by managers and analysts

### Domains

The area the table covers

  * Fact table domains
    * electrofish (includes all electrofishing data)
    * seine (includes all seining and cast netting data)
    * trapnet (includes light trapping, trammel netting and hoop netting data)
    * antenna (Includes all )
    * hydrology
  * Dimenson table domains
    * river 
    * species
    * gear
    * disposition
    * color
  * View domains
    * analysis (datasets intended for external analysis)
    * summary (aggregated and summarised tables)
    * heritage (structured to Heritage upload specs)
    * streams (structured to STReaMS upload specs)
    * intermediate (used in the creation of other views)


