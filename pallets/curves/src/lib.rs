#![cfg_attr(not(feature = "std"), no_std)]

pub use pallet::*;

// #[cfg(test)]
// mod mock;

// #[cfg(test)]
// mod tests;

// #[cfg(feature = "runtime-benchmarks")]
// mod benchmarking;

// TODO: IMPLEMENT THESE DISPATCHABLES!
// create
// buy
// sell

pub mod curves {
    use rust_decimal::prelude::ToPrimitive;
    use rust_decimal::Decimal;
    use frame_support::sp_runtime::traits::IntegerSquareRoot;
    // use schemars::JsonSchema;
    // use std::str::FromStr;
    // use cosmwasm_std::{Decimal as StdDecimal, Uint128};

    pub fn decimal<T: Into<u128>>(num: T, scale: u32) -> Decimal {
        Decimal::from_i128_with_scale(num.into() as i128, scale)
    }

    fn square_root(square: Decimal) -> Decimal {
        // must be even
        // TODO: this can overflow easily at 18... what is a good value?
        const EXTRA_DIGITS: u32 = 12;
        let multiplier = 10u128.saturating_pow(EXTRA_DIGITS);

        // multiply by 10^18 and turn to u128
        let extended = square * decimal(multiplier, 0);
        let extended = extended.floor().to_u128().unwrap();

        // take square root, and build a decimal again
        let root = extended.integer_sqrt();
        decimal(root, EXTRA_DIGITS / 2)
    }

    // #[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, JsonSchema, Default)]
    // #[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Default)]
    pub struct DecimalPlaces {
        /// Number of decimal places for the supply token (this is what was passed in cw20-base instantiate
        pub supply: u32,
        /// Number of decimal places for the reserve token (eg. 6 for uatom, 9 for nstep, 18 for wei)
        pub reserve: u32,
    }

    impl DecimalPlaces {
        pub fn new(supply: u8, reserve: u8) -> Self {
            DecimalPlaces {
                supply: supply as u32,
                reserve: reserve as u32,
            }
        }

        // pub fn to_reserve(self, reserve: Decimal) -> Uint128 {
        pub fn to_reserve(&self, reserve: Decimal) -> u128 {
            let factor = decimal(10u128.pow(self.reserve), 0);
            let out = reserve * factor;
            // TODO: execute overflow better? Result?
            out.floor().to_u128().unwrap().into()
        }

        // pub fn to_supply(self, supply: Decimal) -> Uint128 {
        pub fn to_supply(&self, supply: Decimal) -> u128 {
            let factor = decimal(10u128.pow(self.supply), 0);
            let out = supply * factor;
            // TODO: execute overflow better? Result?
            out.floor().to_u128().unwrap().into()
        }

        // pub fn from_supply(&self, supply: Uint128) -> Decimal {
        pub fn from_supply(&self, supply: u128) -> Decimal {
            decimal(supply, self.supply)
        }

        // pub fn from_reserve(&self, reserve: Uint128) -> Decimal {
        pub fn from_reserve(&self, reserve: u128) -> Decimal {
            decimal(reserve, self.reserve)
        }
    }

    pub trait Curve {
        /// Returns the spot price given the supply.
        /// `f(x)` from the README
        // fn spot_price(&self, supply: Uint128) -> StdDecimal;
        fn spot_price(&self, supply: u128) -> Decimal;

        /// Returns the total price paid up to purchase supply tokens (integral)
        /// `F(x)` from the README
        // fn reserve(&self, supply: Uint128) -> Uint128;
        fn reserve(&self, supply: u128) -> u128;

        /// Inverse of reserve. Returns how many tokens would be issued
        /// with a total paid amount of reserve.
        /// `F^-1(x)` from the README
        // fn supply(&self, reserve: Uint128) -> Uint128;
        fn supply(&self, reserve: u128) -> u128;
    }

    /// spot_price is slope * supply
    pub struct Linear {
        pub slope: Decimal,
        pub normalize: DecimalPlaces,
    }

    impl Linear {
        pub fn new(slope: Decimal, normalize: DecimalPlaces) -> Self {
            Self { slope, normalize }
        }
    }

    impl Curve for Linear {
        fn spot_price(&self, supply: u128) -> Decimal {
            // f(x) = supply * self.value
            let out = self.normalize.from_supply(supply) * self.slope;
            out
        }

        fn reserve(&self, supply: u128) -> u128 {
            // f(x) = self.slope * supply * supply / 2
            let normalized = self.normalize.from_supply(supply);
            let square = normalized * normalized;
            // Note: multiplying by 0.5 is much faster than dividing by 2
            let reserve = square * self.slope * Decimal::new(5, 1);
            self.normalize.to_reserve(reserve)
        }

        fn supply(&self, reserve: u128) -> u128 {
            // f(x) = (2 * reserve / self.slope) ^ 0.5
            // note: use addition here to optimize 2* operation
            let square = self.normalize.from_reserve(reserve + reserve) / self.slope;
            let supply = square_root(square);
            self.normalize.to_supply(supply)
        }
    }
}


#[frame_support::pallet]
pub mod pallet {
    use frame_support::{
        PalletId,
        pallet_prelude::*,
        sp_std::prelude::*,
        sp_runtime::{SaturatedConversion, traits::{AccountIdConversion, Hash}},
        traits::{tokens::ExistenceRequirement, Currency, Randomness},
        transactional,
    };
    use frame_system::pallet_prelude::*;
    use scale_info::TypeInfo;
    use sp_io::hashing::blake2_128;

    type AccountOf<T> = <T as frame_system::Config>::AccountId;
    type BalanceOf<T> =
        <<T as Config>::Currency as Currency<<T as frame_system::Config>::AccountId>>::Balance;

    #[derive(Clone, Encode, Decode, PartialEq, RuntimeDebug, TypeInfo)]
    pub struct BondingCurve<AccountId, CurrencyId> {
        // The module-owned account for this bonding curve.
        // account: AccountId,
        /// The creator of the bonding curve.
        creator: AccountId,
        /// The currency id of the bonding curve token.
        currency_id: CurrencyId,
        /// The exponent of the curve.
        exponent: u32,
        /// The slope of the curve.
        slope: u128,
        /// The maximum supply that can be minted from the curve.
        max_supply: u128,
    }

    impl<AccountId, CurrencyId> BondingCurve<AccountId, CurrencyId> {
        /// Integral when the curve is at point `x`.
        pub fn integral(&self, x: u128) -> u128 {
            let nexp = self.exponent + 1;
            x.pow(nexp) / 1_000_000_000_000 * self.slope / nexp as u128
        }
    }

    #[pallet::pallet]
    #[pallet::generate_store(pub(super) trait Store)]
    // #[pallet::generate_storage_info]
    pub struct Pallet<T>(_);

    #[pallet::config]
    pub trait Config: frame_system::Config {
        /// Because this pallet emits events, it depends on the runtime's definition of an event.
        type Event: From<Event<Self>> + IsType<<Self as frame_system::Config>::Event>;

        /// The Currency handler.
        type Currency: Currency<Self::AccountId>;

        /// The native currency.
        type GetNativeCurrencyId: Get<u64>;

        /// The deposit required for creating a new bonding curve.
        type CurveDeposit: Get<BalanceOf<Self>>;

        /// The module identifier.
        type PalletId: Get<PalletId>;
    }

    #[pallet::error]
    pub enum Error<T> {
        /// Sender does not have enough base currency to reserve for a new curve.
        InsufficientBalanceToReserve,
        /// A curve does not exist for this curve id.
        CurveDoesNotExist,
        /// Sender does not have enough base currency to make a purchase.
        InsufficentBalanceForPurchase,
        /// The currency that is trying to be created already exists.
        CurrencyAlreadyExists,
    }

    #[pallet::event]
    #[pallet::generate_deposit(pub(super) fn deposit_event)]
    pub enum Event<T: Config> {
        /// (CurveId, Creator)
        NewCurve(u64, T::AccountId),
        /// (Buyer, CurveId, Amount, Cost)
        CurveBuy(T::AccountId, u64, BalanceOf<T>, BalanceOf<T>),
        /// (Seller, CurveId, Amount, Return)
        CurveSell(T::AccountId, u64, BalanceOf<T>, BalanceOf<T>),
    }

    #[pallet::storage]
    #[pallet::getter(fn curves)]
    /// Keeps track of the number of items in existence.
    pub(super) type Curves<T: Config> =
        StorageMap<_, Twox64Concat, u64, Option<BondingCurve<T::AccountId, u64>>>;

    #[pallet::storage]
    // #[pallet::getter(fn next_curve_id)]
    /// Keeps track of the number of Kitties in existence.
    pub(super) type NextCurveId<T: Config> = StorageValue<_, u64, ValueQuery>;

    #[pallet::call]
    impl<T: Config> Pallet<T> {
        #[pallet::weight(100)]
        pub fn create(origin: OriginFor<T>, currency_id: u64, exponent: u32, slope: u128, max_supply: u128, initial_supply: u128) -> DispatchResult {
            let sender = ensure_signed(origin)?;
            // Requires an amount to be reserved.
            // ensure!(
            //     T::Currency::can_reserve(T::GetNativeCurrencyId::get(), &sender, T::CurveDeposit::get()),
            //     Error::<T>::InsufficientBalanceToReserve,
            // );

            // Ensure that a curve with this id does not already exist.
            // ensure!(
            //     T::Currency::total_issuance(currency_id) == 0.into(),
            //     Error::<T>::CurrencyAlreadyExists,
            // );

            // Adds 1 of the token to the module account.
            // T::Currency::deposit(currency_id, &Self::module_account(), 1.saturated_into())?;

            // T::Currency::deposit(currency_id, &sender, initial_supply.saturated_into())?;

            let new_curve = BondingCurve {
                creator: sender.clone(),
                currency_id,
                exponent,
                slope,
                max_supply,
            };

            // Mutations start here
            let curve_id = Self::next_id();
            <Curves<T>>::insert(curve_id, new_curve);
            Ok(())
        }

        #[pallet::weight(100)]
        pub fn buy(origin: OriginFor<T>) -> DispatchResult {
            let sender = ensure_signed(origin)?;
            Ok(())
        }

        #[pallet::weight(100)]
        pub fn sell(origin: OriginFor<T>) -> DispatchResult {
            let sender = ensure_signed(origin)?;
            Ok(())
        }
    }

    impl<T: Config> Pallet<T> {
        fn module_account() -> T::AccountId {
            T::PalletId::get().into_account()
        }

        fn get_module_sub_account(id: u64) -> T::AccountId {
            T::PalletId::get().into_sub_account(id)
        }

        /// DANGER - Mutates storage
        fn next_id() -> u64 {
            let id = <NextCurveId<T>>::get();
            <NextCurveId<T>>::mutate(|n| *n += 1);
            id
        }
    }
}
