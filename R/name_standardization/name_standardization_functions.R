

# Function to handle basic bank name standardization
standardize_bank_names <- function(name) {
  case_when(
    # Chinese Banks
    str_detect(
      name,
      "(?i)ICBC|Industrial and Commercial Bank of China"
    ) ~ "ICBC",
    
    # Bank of China (including BOC)
    str_detect(
      name,
      "(?i)BOC\\s+Aviation|Bank\\s+of\\s+China"
    ) & !str_detect(
      name, 
      "(?i)Agricultural|Export|Postal|People"
    ) ~ "Bank of China",
    
    str_detect(
      name,
      "(?i)^Agricultural Bank of China"
    ) ~ "Agricultural Bank of China",
    
    str_detect(
      name,
      "(?i)Export-Import Bank of China|China Eximbank"
    ) ~ "Export-Import Bank of China",
    
    str_detect(
      name,
      "(?i)Postal Savings Bank of China|\\bPSBC\\b"
    ) ~ "Postal Savings Bank of China",
    
    str_detect(
      name,
      "(?i)People's Bank of China|\\bPBC\\b"
    ) ~ "People's Bank of China",
    
    str_detect(
      name,
      "(?i)China Development Bank|CDB"
    ) ~ "China Development Bank",
    
    str_detect(
      name,
      "(?i)China Construction Bank|CCB"
    ) ~ "China Construction Bank",
    
    # CITIC Group
    str_detect(
      name,
      "(?i)CITIC|China International Trust"
    ) ~ "CITIC",
    
    str_detect(
      name,
      "(?i)Bank of Communications|BoCom|BoComm"
    ) ~ "Bank of Communications",
    
    # Middle Eastern Banks
    # Arabn Banking Corporation
    str_detect(
      name,
      "(?i)Arab\\s+Banking\\s+Corporation|\\bBank\\s+ABC\\b"
    ) ~ "Bank ABC",
    
    # Banco ABC (Brazil)
    str_detect(
      name,
      "(?i)Banco\\s+ABC"
    ) ~ "Banco ABC",
    
    str_detect(
      name,
      paste0(
        "(?i)Qatar\\s+National\\s+Bank|\\bQNB\\b|",
        "PT\\s+Bank\\s+QNB\\s+Indonesia"
      )
    ) & !str_detect(
      name,
      "(?i)CaixaBank|Scotiabank|Odea|DGB|Deka"
    ) ~ "QNB",
    
    # International Banks
    str_detect(name, "(?i)HSBC") ~ "HSBC",
    
    str_detect(
      name,
      "(?i)Bank of America|BofA"
    ) ~ "Bank of America",
    
    str_detect(
      name,
      "(?i)Citi(bank|group)"
    ) ~ "Citibank",
    
    str_detect(
      name,
      "(?i)BNP Paribas|Fortis Bank"
    ) ~ "BNP Paribas",
    
    str_detect(name, "(?i)Deutsche Bank") ~ "Deutsche Bank",
    
    str_detect(
      name,
      "(?i)Standard Chartered"
    ) ~ "Standard Chartered",
    
    str_detect(
      name,
      paste0(
        "(?i)Mitsubishi.*UFJ|MUFG|Mitsubishi HC Capital|",
        "UFJ Bank|Bank of Tokyo-Mitsubishi|BTM"
      )
    ) ~ "Mitsubishi UFJ",
    
    str_detect(
      name,
      "(?i)^ING|ING-|Internationale Nederlanden Groep"
    ) ~ "ING",
    
    str_detect(name, "(?i)UniCredit") ~ "UniCredit",
    
    str_detect(
      name,
      "(?i)Sumitomo|SMBC"
    ) ~ "Sumitomo Mitsui",
    
    str_detect(
      name,
      "(?i)Standard Bank|Stanbic|Standard Finance.*Isle of Man"
    ) ~ "Standard Bank",
    
    str_detect(
      name,
      "(?i)Germany Development Bank|KFW"
    ) ~ "KfW",
    
    str_detect(
      name,
      "(?i)Absa|ABGL"
    ) ~ "Absa",
    
    # ANZ with complete exclusions
    str_detect(
      name,
      "(?i)^ANZ|Australia and New Zealand Banking"
    ) & !str_detect(
      name, 
      "(?i)Ganzhou|Chinese Business"
    ) ~ "ANZ",
    
    str_detect(
      name,
      "(?i)Itaú|Itau|Banco Itaú"
    ) ~ "Itau",
    
    str_detect(name, "(?i)Dexia") ~ "Dexia",
    
    str_detect(name, "(?i)KBC") ~ "KBC",
    
    str_detect(
      name,
      paste0(
        "(?i)Bank of New York Mellon|\\bBNY Mellon\\b|",
        "^Bank of New York$"
      )
    ) & !str_detect(name, "(?i)Safra") ~ "BNY Mellon",
    
    str_detect(name, "(?i)Safra") ~ "Safra Bank",
    
    str_detect(
      name,
      "(?i)Natexis Banques Populaires"
    ) ~ "Natixis",
    
    str_detect(name, "(?i)Mizuho") ~ "Mizuho",
    
    str_detect(name, "(?i)Ecobank") ~ "Ecobank",
    
    str_detect(name, "(?i)Gazprombank") ~ "Gazprombank",
    
    str_detect(
      name,
      "(?i)Société Générale|Societe Generale|Société General"
    ) ~ "Societe Generale",
    
    str_detect(name, "(?i)Barclay(s|'s)") ~ "Barclays",
    
    str_detect(name, "(?i)Goldman Sachs") ~ "Goldman Sachs",
    
    str_detect(
      name,
      "(?i)JPMorgan|J\\.?\\s*P\\.?\\s*Morgan"
    ) ~ "JPMorgan",
    
    str_detect(
      name,
      "(?i)Nomura(?:.*Holdings|.*Trust.*Banking)"
    ) ~ "Nomura",
    
    str_detect(name, "(?i)^UBS") ~ "UBS",
    
    str_detect(name, "(?i)Commerzbank") ~ "Commerzbank",
    
    str_detect(
      name,
      "(?i)Crédit Agricole|Credit Agricole|Crédit Lyonnais|CACIB|Calyon"
    ) ~ "Credit Agricole",
    
    str_detect(
      name,
      "(?i)BBVA|Banco Bilbao Vizcaya Argentaria"
    ) ~ "BBVA",
    
    str_detect(name, "(?i)Raiffeisen") ~ "Raiffeisen Bank",
    
    str_detect(
      name,
      "(?i)RMB Holdings|Rand Merchant Bank"
    ) ~ "Rand Merchant Bank",
    
    str_detect(name, "(?i)Rabobank") ~ "Rabobank",
    
    str_detect(
      name,
      "(?i)Mega International Commercial Bank"
    ) ~ "Mega International Commercial Bank",
    
    str_detect(name, "(?i)Nordea") ~ "Nordea",
    
    str_detect(name, "(?i)Nedbank") ~ "Nedbank",
    
    str_detect(
      name,
      "(?i)Intesa Sanpaolo"
    ) ~ "Intesa Sanpaolo",
    
    str_detect(
      name,
      "(?i)Yap[iı] Kredi"
    ) ~ "Yapi Kredi",
    
    str_detect(
      name,
      "(?i)Toronto-Dominion|TD Bank"
    ) ~ "TD Bank",
    
    str_detect(name, "(?i)VTB") ~ "VTB Bank",
    
    str_detect(name, "(?i)CIMB") ~ "CIMB",
    
    str_detect(
      name,
      "(?i)CaixaBank|Criteria CaixaCorp"
    ) ~ "CaixaBank",
    
    str_detect(name, "(?i)Odeabank") ~ "Odeabank",
    
    str_detect(
      name,
      "(?i)Vak[iı]fBank|Vakif Katilim"
    ) ~ "VakifBank",
    
    str_detect(
      name,
      "(?i)İşbank|Isbank"
    ) ~ "Isbank",
    
    str_detect(
      name,
      "(?i)Ziraat|Türkiye Ziraat"
    ) ~ "Ziraat Bank",
    
    str_detect(
      name,
      "(?i)Halk.*Bank|Demir-Halk"
    ) ~ "HalkBank",
    
    str_detect(
      name,
      "(?i)Wells Fargo"
    ) ~ "Wells Fargo",
    
    str_detect(
      name,
      "(?i)Credit Suisse|BankBoston"
    ) ~ "Credit Suisse",
    
    str_detect(
      name,
      "(?i)ABN.?A[m]ro"
    ) ~ "ABN Amro",
    
    str_detect(name, "(?i)Sberbank") ~ "Sberbank",
    
    str_detect(
      name,
      "(?i)Bank of Nova Scotia|\\bScotiabank\\b|\\bBNS\\b"
    ) ~ "Scotiabank",
    
    str_detect(
      name,
      "(?i)Royal Bank of Scotland|RBS"
    ) ~ "Royal Bank of Scotland",
    
    str_detect(
      name,
      "(?i)Bank of Montreal|BMO"
    ) ~ "Bank of Montreal",
    
    str_detect(name, "(?i)Erste Bank") ~ "Erste Bank",
    
    str_detect(
      name,
      paste0(
        "(?i)(?:PT\\.?\\s+)?Bank\\s+Mandiri",
        "(?:\\s+Tbk)?(?:\\s*,?\\s*[^,]+Branch)?"
      )
    ) & !str_detect(name, "(?i)Syariah") ~ "Bank Mandiri",
    
    str_detect(
      name,
      "(?i)(?:PT\\.?\\s+)?Bank\\s+Mandiri\\s+Syariah"
    ) ~ "Bank Mandiri Syariah",
    
    # United Overseas Bank Group
    str_detect(
      name,
      paste0(
        "(?i)United\\s+Overseas\\s+Bank|",
        "\\bUOB\\b(?:.*Limited|.*Indonesia|.*Singapore)?"
      )
    ) ~ "United Overseas Bank",
    
    # DBS Bank Group
    str_detect(
      name,
      "(?i)DBS\\s+Bank|Development\\s+Bank\\s+of\\s+Singapore|PT\\s+Bank\\s+DBS"
    ) ~ "DBS Bank",
    
    # Bank Negara Indonesia
    str_detect(
      name,
      "(?i)Bank\\s+Negara\\s+Indonesia|PT\\s+Bank\\s+Negara\\s+Indonesia"
    ) ~ "Bank Negara Indonesia",
    
    # Bank Negara Indonesia
    str_detect(
      name,
      "(?i)Bank\\s+Negara\\s+Malaysia"
    ) ~ "Bank Negara Malaysia",
    
    # DZ Bank Group
    str_detect(
      name,
      "(?i)\\bDZ\\s+Bank|WGZ\\s+Bank"
    ) ~ "DZ Bank",
    
    # Development Banks
    str_detect(
      name,
      paste0(
        "(?i)^African Development (Bank|Fund)|",
        "^Africa Development Bank|\\bAfDB\\b"
      )
    ) & !str_detect(name, "(?i)West African|East African") ~ 
      "African Development Bank",
    
    str_detect(
      name,
      "(?i)Inter-American.*Bank|IDB Invest"
    ) ~ "Inter-American Development Bank",
    
    str_detect(
      name,
      "(?i)International Finance Corporation|IFC"
    ) ~ "International Finance Corporation",
    # Keep original if no match
    TRUE ~ name
  )
}



# Function to handle government and state institution names
standardize_government_names <- function(name) {
  # Define provinces and cities once
  provinces_pattern <- paste0(
    "(?i)",
    paste(
      c(
        "Yunnan",
        "Guangdong",
        "Sichuan",
        "Jiangsu",
        "Zhejiang"
      ),
      collapse = "|"
    )
  )
  
  cities_pattern <- paste0(
    "(?i)",
    paste(
      c(
        "Shanghai",
        "Beijing",
        "Guangzhou",
        "Shenzhen"
      ),
      collapse = "|"
    )
  )
  
  case_when(
    # Ministries
    str_detect(
      name,
      "(?i)China Ministry of Commerce"
    ) ~ "Ministry of Commerce",
    
    str_detect(
      name,
      "(?i)China Ministry of Foreign Affairs"
    ) ~ "Ministry of Foreign Affairs",
    
    str_detect(
      name,
      "(?i)China Ministry of Education"
    ) ~ "Ministry of Education",
    
    # Provincial Governments
    str_detect(
      name,
      paste0(
        "(?i)Provincial Government|Province Municipality|",
        "People's Government of.*Province"
      )
    ) & str_detect(name, provinces_pattern) ~ {
      province_name <- str_extract(
        name,
        provinces_pattern
      )
      paste(province_name, "Provincial Government")
    },
    
    # Municipal Governments
    str_detect(
      name,
      paste0(
        "(?i)Municipal Government|Municipality|City Government|",
        "Municipal People's Government"
      )
    ) & str_detect(name, cities_pattern) ~ {
      city_name <- str_extract(
        name,
        cities_pattern
      )
      paste(city_name, "Municipal Government")
    },
    
    # Military/Defense
    str_detect(
      name,
      paste0(
        "(?i)(Chinese\\s+)?People'?s\\s+Liberation\\s+Army|",
        "\\bPLA\\b|Chinese Army"
      )
    ) & !str_detect(name, "(?i)Air Force") ~ "People's Liberation Army",
    
    str_detect(
      name,
      "(?i)People'?s\\s+Liberation\\s+Army\\s+Air\\s+Force"
    ) ~ "People's Liberation Army Air Force",
    
    # Diplomatic Missions
    str_detect(
      name,
      "(?i)Chinese Embassy|Chinese Consulate"
    ) ~ "Chinese Diplomatic Mission",
    
    # State Institutions
    str_detect(
      name,
      "(?i)Unspecified Chinese Government Institution"
    ) ~ "Unspecified Chinese Government Institution",
    
    # Universities
    str_detect(
      name,
      "(?i)University"
    ) ~ str_replace(name, "(?i)\\s+of\\s+China", ""),
    
    # Keep original if no match
    TRUE ~ name
  )
}

# Function to handle state-owned companies
standardize_soe_names <- function(name) {
  case_when(
    # Energy Companies
    str_detect(
      name,
      "(?i)China National Petroleum Corporation|CNPC"
    ) ~ "China National Petroleum Corporation",
    
    str_detect(name, "(?i)Sinopec") ~ "Sinopec",
    
    str_detect(
      name,
      "(?i)State Grid Corporation of China|SGCC"
    ) ~ "State Grid Corporation of China",
    
    # Infrastructure & Construction
    str_detect(
      name,
      "(?i)China Railway.*(Corporation|Group)|CRCC|CREC"
    ) ~ "China Railway Group",
    
    str_detect(
      name,
      "(?i)Sinohydro|PowerChina"
    ) ~ "PowerChina",
    
    # State Power Investment Corporation
    str_detect(
      name,
      paste0(
        "(?i)State\\s+Power\\s+Investment|",
        "SPIC|State\\s+Power\\s+Corporation"
      )
    ) ~ "State Power Investment Corporation",
    
    # State Grid
    str_detect(
      name,
      "(?i)State\\s+Grid|SGCC"
    ) ~ "State Grid",
    
    # Financial Institutions & Insurance
    str_detect(
      name,
      "(?i)China Life Insurance"
    ) ~ "China Life Insurance",
    
    str_detect(
      name,
      "(?i)China Investment Corporation|CIC"
    ) ~ "China Investment Corporation",
    
    str_detect(
      name,
      "(?i)Silk Road Fund"
    ) ~ "Silk Road Fund",
    
    # Telecommunications
    str_detect(
      name,
      "(?i)China Mobile|CMC"
    ) ~ "China Mobile",
    
    str_detect(
      name,
      "(?i)China Telecom"
    ) ~ "China Telecom",
    
    str_detect(
      name,
      "(?i)China Unicom"
    ) ~ "China Unicom",
    
    # Aviation & Transportation
    str_detect(
      name,
      "(?i)Air China"
    ) ~ "Air China",
    
    str_detect(
      name,
      "(?i)China Eastern Airlines"
    ) ~ "China Eastern Airlines",
    
    str_detect(
      name,
      "(?i)China Southern Airlines"
    ) ~ "China Southern Airlines",
    
    str_detect(
      name,
      "(?i)COSCO|China Ocean Shipping"
    ) ~ "COSCO",
    
    # Resources & Mining
    str_detect(
      name,
      "(?i)China Minmetals"
    ) ~ "China Minmetals",
    
    str_detect(
      name,
      "(?i)China National Gold|CNG"
    ) ~ "China National Gold",
    
    # Nuclear
    str_detect(
      name,
      "(?i)China National Nuclear Corporation|CNNC"
    ) ~ "China National Nuclear Corporation",
    
    str_detect(
      name,
      "(?i)China General Nuclear|CGN"
    ) ~ "China General Nuclear",
    
    # Agricultural
    str_detect(
      name,
      "(?i)COFCO|China National Cereals"
    ) ~ "COFCO",
    
    # Heavy Industry
    str_detect(
      name,
      "(?i)China State Shipbuilding Corporation|CSSC"
    ) ~ "China State Shipbuilding Corporation",
    
    str_detect(
      name,
      "(?i)China North Industries|NORINCO"
    ) ~ "NORINCO",
    
    # Chemical Industry
    str_detect(
      name,
      "(?i)ChemChina|China National Chemical"
    ) ~ "ChemChina",
    
    str_detect(
      name,
      "(?i)Sinochem"
    ) ~ "Sinochem",
    
    # Development Zones & Investment
    # China Merchants Group
    str_detect(
      name,
      "(?i)China\\s+Merchants"
    ) ~ "China Merchants Group",
    
    # Keep original if no match
    TRUE ~ name
  )
}

# Function to clean up corporate identifiers
clean_corporate_identifiers <- function(name) {
  name |>
    # Remove common Ltd/Limited variations with more specific patterns
    str_replace_all("(?i)\\s+Ltd\\.?,?(?:\\s|$)", " ") |>
    str_replace_all("(?i)\\s+Limited,?(?:\\s|$)", " ") |>
    str_replace_all("(?i)\\s+Co\\.?,?\\s*Ltd\\.?,?(?:\\s|$)", " ") |>
    
    # Remove PLC/PJSC/PSC/PCL variations
    str_replace_all("(?i)\\s+P\\.?L\\.?C\\.?,?(?:\\s|$)", " ") |>
    str_replace_all("(?i)\\s+P\\.?J\\.?S\\.?C\\.?,?(?:\\s|$)", " ") |>
    str_replace_all("(?i)\\s+P\\.?S\\.?C\\.?,?(?:\\s|$)", " ") |>
    str_replace_all("(?i)\\s+P\\.?C\\.?L\\.?,?(?:\\s|$)", " ") |>
    
    # Remove SA/SpA variations with careful spacing
    str_replace_all("(?i)\\s+S\\.?\\s*A\\.?,?(?:\\s|$)", " ") |>
    str_replace_all("(?i)\\s+S\\.?p\\.?A\\.?,?(?:\\s|$)", " ") |>
    str_replace_all("(?i)\\s+S\\.?A\\.?K\\.?,?(?:\\s|$)", " ") |>
    
    # Remove LLC variations
    str_replace_all("(?i)\\s+L\\.?L\\.?C\\.?,?(?:\\s|$)", " ") |>
    
    # Remove AG/AB/AS/A/S variations
    str_replace_all("(?i)\\s+A\\.?G\\.?,?(?:\\s|$)", " ") |>
    str_replace_all("(?i)\\s+A\\.?B\\.?,?(?:\\s|$)", " ") |>
    str_replace_all("(?i)\\s+A\\.?S\\.?,?(?:\\s|$)", " ") |>
    str_replace_all("(?i)\\s+ASA,?(?:\\s|$)", " ") |>
    
    # Remove shorter Corp/Co variations but keep "Corporation"
    str_replace_all("(?i)\\s+Corp\\.?,?(?:\\s|$)", " ") |>
    
    # Remove BHD variations
    str_replace_all("(?i)\\s+BHD,?(?:\\s|$)", " ") |>
    str_replace_all("(?i)\\s+Bhd,?(?:\\s|$)", " ") |>
    
    # Remove GmbH/mbH variations
    str_replace_all("(?i)\\s+GmbH,?(?:\\s|$)", " ") |>
    str_replace_all("(?i)\\s+mbH,?(?:\\s|$)", " ") |>
    
    # Remove NV/N.A. variations
    str_replace_all("(?i)\\s+N\\.?V\\.?,?(?:\\s|$)", " ") |>
    str_replace_all("(?i)\\s+N\\.?A\\.?,?(?:\\s|$)", " ") |>
    
    # Remove Inc variations
    str_replace_all("(?i)\\s+Incorporated,?(?:\\s|$)", " ") |>
    str_replace_all("(?i)\\s+Inc\\.?,?(?:\\s|$)", " ") |>
    
    # Remove Turkish Joint Stock Company indicator
    str_replace_all("(?i)\\s+T\\.?A\\.?Ş\\.?,?(?:\\s|$)", " ") |>
    
    # Remove Bahrain Company indicator
    str_replace_all("(?i)\\s+B\\.?S\\.?C\\.?,?(?:\\s|$)", " ") |>
    
    # Remove Pte/Private variations
    str_replace_all("(?i)\\s+Pte\\.?,?(?:\\s|$)", " ") |>
    str_replace_all("(?i)\\s+Private\\.?,?(?:\\s|$)", " ") |>
    
    # Remove Indonesian PT and Tbk
    str_replace_all("(?i)^PT\\s+", "") |>
    str_replace_all("(?i)\\s+Tbk\\.?,?(?:\\s|$)", " ") |>
    
    # Handle Group/Bank Group variations
    str_replace_all("(?i)\\s+Group\\s+Bank(?:\\s|$)", " Bank") |>
    str_replace_all("(?i)\\s+Group,?(?:\\s|$)", " ") |>
    
    # Remove Company/Co variations
    str_replace_all("(?i)\\s+Company,?(?:\\s|$)", " ") |>
    str_replace_all("(?i)\\s+Co\\.?,?(?:\\s|$)", " ") |>
    
    # Only remove parentheses if surrounded by whitespace
    str_replace_all("\\s+\\([^)]*\\)\\s+", " ") |>
    str_replace_all("\\s+\\([^)]*\\)$", "") |>
    
    # Clean up extra punctuation and whitespace
    str_replace_all("\\s*,\\s*$", "") |>
    str_replace_all("\\s*\\.\\s*$", "") |>
    str_replace_all(",\\s*\\.", "") |>
    str_replace_all("\\s+,", ",") |>
    str_trim() |>
    str_squish()
}

# Main standardization function that combines all the above
standardize_institution_names <- function(name) {
  name |>
    # Apply standardization functions in sequence
    standardize_bank_names() |>
    standardize_government_names() |>
    standardize_soe_names() |>
    clean_corporate_identifiers()
}

# Fix the wrapper function to properly handle column names
standardize_all_names <- function(df, name_column) {
  # Input validation
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }
  
  name_col_enquo <- enquo(name_column)
  
  if (!quo_name(name_col_enquo) %in% colnames(df)) {
    stop("Specified name column does not exist in the data frame")
  }
  
  df |>
    mutate(
      standardized_name = standardize_institution_names(!!name_col_enquo)
    )
}

# Helper function to create a matching dictionary
create_name_dictionary <- function(df, original_col, standardized_col) {
  df |>
    select({{original_col}}, {{standardized_col}}) |>
    distinct() |>
    arrange({{standardized_col}}, {{original_col}})
}

# Function to check for unmatched patterns
check_unmatched_names <- function(df, name_column) {
  df |>
    filter({{name_column}} == standardized_name) |>
    pull({{name_column}}) |>
    unique() |>
    sort()
}